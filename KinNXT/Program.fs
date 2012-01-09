open LegoBot
open Microsoft.Research.Kinect.Nui

printfn "Connecting..."
let bot = new LegoBot "COM3"

printfn "Initializing Kinect..."
if Runtime.Kinects.Count = 0 then failwith "Kinect missing"
let kinect = Runtime.Kinects.Item 0
kinect.Initialize(RuntimeOptions.UseDepth ||| RuntimeOptions.UseSkeletalTracking)
kinect.SkeletonEngine.IsEnabled <- true
kinect.SkeletonEngine.TransformSmooth <- true
kinect.SkeletonFrameReady.Add(fun frame ->
    let drive port position =
        let power = position * 2.f * 100.f |> int
        bot.SetOutputState power port OutputMode.MotorOn RegulationMode.Idle 0 RunState.Running 0ul
    let joints = frame.SkeletonFrame.Skeletons.[0].Joints
    let left = joints.[JointID.HandLeft]
    let right = joints.[JointID.HandRight]
    printfn "Left: %A Right %A" left right
    drive 0 left.Position.Y
    drive 2 right.Position.Y)

System.Console.ReadLine() |> ignore

bot.Disconnect()