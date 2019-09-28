{
  DemoWidget {
    type: ImageDisplayWidget
    image: "ui/islandImage.png"
    scalingStyle: Scale(1)
    positionStyle: Center
    color: [1.0, 1.0, 1.0, 1.0]

    position : ["centered", "centered"]
    width : intrinsic
    height : intrinsic

    drawBackground: true
    backgroundColor: [1.0, 1.0, 1.0, 1.0]

    children: [
      {
        type : TextDisplayWidget
        text: [
          {image: "ui/hammer.png"},
          {text: "Test", color: [0.1, 0.7, 0.2, 1.0]},
          {image: "ui/hammer.png", color: [1.0, 0.8, 0.8, 1.0]}
        ]
      }
    ]
  }
}
