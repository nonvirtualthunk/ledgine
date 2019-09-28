{
  ExampleWidget {
    type: ImageDisplayWidget
    image: "ui/islandImage.png"
    scalingStyle: ScaleToFit
    positionStyle: Center
    color: [1.0, 1.0, 1.0, 1.0]

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
