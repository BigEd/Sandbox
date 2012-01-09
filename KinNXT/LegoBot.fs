module LegoBot

open System
open System.IO
open System.IO.Ports
open System.Text

type SensorKind =
    | None          = 0x0
    | Switch        = 0x1
    | Temperature   = 0x2
    | Reflection    = 0x3
    | Angle         = 0x4
    | LightActive   = 0x5
    | LightInactive = 0x6
    | SoundDB       = 0x7
    | SoundDBA      = 0x8
    | Custom        = 0x9
    | LowSpeed      = 0xA
    | LowSpeed9V    = 0xB
    | Color         = 0xD

type SensorMode =
    | Raw             = 0x00
    | Boolean         = 0x20
    | TransitionCount = 0x40
    | PeriodCounter   = 0x60
    | PCTFullScale    = 0x80
    | Celsius         = 0xA0
    | Fahrenheit      = 0xC0
    | AngleSteps      = 0xE0
    | SlopeMask       = 0x1F

type InputValue = {
    IsValid      : bool
    IsCalibrated : bool
    Kind         : SensorKind
    Mode         : SensorMode
    Raw          : int
    Normalized   : int
    Scaled       : int
    Calibrated   : int }

[<Flags>]
type OutputMode =
    | None      = 0
    | MotorOn   = 1
    | Brake     = 2
    | Regulated = 4

type RegulationMode =
    | Idle       = 0
    | MotorSpeed = 1
    | MotorSync  = 2

[<Flags>]
type RunState =
    | Idle     = 0x00
    | RampUp   = 0x10
    | Running  = 0x20
    | RampDown = 0x40

type OutputState = {
    Power         : int
    Mode          : OutputMode
    Regulation    : RegulationMode
    Turn          : int
    Run           : RunState
    Limit         : uint32
    TachoCount    : int
    BlockCount    : int
    RotationCount : int }

type DeviceInfo = {
    Name      : string
    BTAddress : byte[]
    Signal    : int32
    Memory    : int32 }

type VersionInfo = {
    Protocol : float
    Firmware : float }

type LegoBot(port : string) =
    let reader, writer =
        let com = new SerialPort(port)
        com.Open()
        com.ReadTimeout <- 1500
        com.WriteTimeout <- 1500
        let stream = com.BaseStream
        new BinaryReader(stream), new BinaryWriter(stream)

    let send (message : byte[]) =
        int16 message.Length |> writer.Write
        writer.Write message
        writer.Flush()

    let expect (bytes : byte[]) =
        let actual = reader.ReadBytes bytes.Length
        if actual <> bytes then failwith "Invalid response"

    let file (name : string) =
        if name.Length > 19 then failwith "Name too long."
        let bytes = (Seq.map byte name |> List.ofSeq)
        bytes @ List.init (20 - bytes.Length) (fun _ -> 0uy)

    let bytesToString bytes =
        let len = Array.findIndex ((=) 0uy) bytes
        Encoding.ASCII.GetString(bytes, 0, len)

    let intToBytes i = [byte i; i >>> 8 |> byte; i >>> 16 |> byte; i >>> 24 |> byte]

    let shortToBytes (s : int16) = [byte s; s >>> 8 |> byte]

    member x.KeepAlive () = send [|0x80uy; 0x80uy; 0x0Duy|]

    member x.GetDeviceInfo () =
        send [|1uy; 0x9Buy|]
        expect [|33uy; 0uy; 2uy; 0x9Buy; 0uy|]
        { Name = Encoding.ASCII.GetString(reader.ReadBytes 15)
          BTAddress = reader.ReadBytes 7
          Signal = reader.ReadInt32()
          Memory = reader.ReadInt32() }

    member x.GetVersion () =
        send [|1uy; 0x88uy|]
        expect [|7uy; 0uy; 2uy; 0x88uy; 0uy|]
        let readMajorMinor () = Double.Parse(sprintf "%i.%i" (reader.ReadByte()) (reader.ReadByte()))
        { Protocol = readMajorMinor (); Firmware = readMajorMinor () }

    member x.GetBatteryLevel () =
        send [|0uy; 0xBuy|]
        expect [|5uy; 0uy; 2uy; 0xBuy; 0uy|]
        (reader.ReadInt16() |> float) / 1000.

    member x.SetBrickName (name : string) =
        let truncated = Seq.map byte name |> Seq.take (min name.Length 15) |> List.ofSeq
        [1uy; 0x98uy] @ truncated @ [byte truncated.Length] |> Array.ofList |> send
        expect [|3uy; 0uy; 2uy; 0x98uy; 0uy|]

    member x.PlayTone frequency (duration : TimeSpan) =
        writer.Write [|6uy; 0uy; 0x80uy; 3uy|]
        int16 frequency |> writer.Write
        int16 duration.TotalMilliseconds |> writer.Write
        writer.Flush()

    member x.SetInputMode port (kind : SensorKind) (mode : SensorMode) =
        send [|0x80uy; 5uy; byte port; byte kind; byte mode|]

    member x.GetInputValues port =
        send [|0uy; 7uy; byte port|]
        expect [|16uy; 0uy; 2uy; 7uy; 0uy|]
        reader.ReadByte() |> ignore
        { IsValid = (reader.ReadByte() = 1uy)
          IsCalibrated = (reader.ReadByte() = 1uy)
          Kind = reader.ReadByte() |> int |> enum
          Mode = reader.ReadByte() |> int |> enum
          Raw = reader.ReadInt16() |> int
          Normalized = reader.ReadInt16() |> int
          Scaled = reader.ReadInt16() |> int
          Calibrated = reader.ReadInt16() |> int }

    member x.ResetInputScaledValue port = send [|0x80uy; 8uy; byte port|]

    member x.SetOutputState port power (mode : OutputMode) (regulation : RegulationMode) turn (run : RunState) (limit : uint32) = // port 0xFF means 'all'
        writer.Write [|12uy; 0uy; 0uy; 4uy; byte port; byte power; byte mode; byte regulation; byte turn; byte run|]
        writer.Write limit
        writer.Flush()
        expect [|3uy; 0uy; 2uy; 4uy; 0uy|]

    member x.GetOutputState port =
        send [|0uy; 6uy; byte port|]
        expect [|25uy; 0uy; 2uy; 6uy; 0uy|]
        reader.ReadByte() |> ignore
        { Power = reader.ReadByte() |> int32
          Mode = reader.ReadByte() |> int32 |> enum
          Regulation = reader.ReadByte() |> int32 |> enum
          Turn = reader.ReadByte() |> int32
          Run = reader.ReadByte() |> int32 |> enum
          Limit = reader.ReadUInt32()
          TachoCount = reader.ReadInt32()
          BlockCount = reader.ReadInt32()
          RotationCount = reader.ReadInt32() }    

    member x.ResetMotorPosition port relative =
        send [|0x80uy; 0xAuy; byte port; (if relative then 1uy else 0uy)|]

    member x.MessageWrite box (message : string) =
        let truncated = Seq.map byte message |> Seq.take (min message.Length 59) |> List.ofSeq
        [0x0uy; 0x09uy; byte box] @ [byte truncated.Length + 1uy] @ truncated @ [0uy] |> Array.ofList |> send
        expect [|3uy; 0uy; 2uy; 0x09uy; 0uy|]

    member x.StartProgram name =
        [0uy; 0uy] @ file name |> Array.ofList |> send
        expect [|3uy; 0uy; 2uy; 0uy; 0uy|]

    member x.StopProgram () =
        send [|0uy; 1uy|]
        expect [|3uy; 0uy; 2uy; 1uy; 0uy|]

    member x.Disconnect () =
        List.iter (fun p -> x.SetInputMode p SensorKind.None SensorMode.Raw) [0..3]
        List.iter (fun p -> x.SetOutputState p 0 OutputMode.MotorOn RegulationMode.Idle 0 RunState.Idle 0ul) [0..2]
        reader.Close()
        writer.Close()