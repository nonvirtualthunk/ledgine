DemoWidget {
  type: ImageDisplayWidget
  image: "ui/islandImage.png"
  scalingStyle: Scale(1)
  positionStyle: Center
  color: [1.0, 1.0, 1.0, 1.0]

  position: ["centered", "centered"]
  width: intrinsic
  height: 500

  drawBackground: true
  backgroundColor: [1.0, 1.0, 1.0, 1.0]

  children: {
    HammerText: {
      type: TextDisplayWidget
//      text: [
//        {image: "ui/hammer.png"},
//        {text: "Test", color: [0.1, 0.7, 0.2, 1.0]},
//        {image: "ui/hammer.png", color: [1.0, 0.8, 0.8, 1.0]}
//      ],
      text: "Test"
      width : intrinsic
      height : intrinsic
      position : ["centered", "10 from Bottom"]
      drawBackground : false
      fontScale : 1.5
    }
    HammerImage: {
      type: ImageDisplayWidget
      image: "%(demoBinding.image)"
      scalingStyle: Scale(1)
      positionStyle: Center
      color: [1, 1, 1, 1]
      position: ["centered", "10"]
      width: intrinsic
      height: intrinsic

      drawBackground: false
    }
  }
}

DemoText {
  type: TextDisplayWidget
  text: "%(demoBinding.icon) %(demoBinding.mode) : %(demoBinding.presses) %(demoBinding.icon)"
  defaultText: "No presses yet"
  dataBinding: "%(demoBinding.textData)"

  background.image: "ui/minimalistBorder_ne.png"

  position: [centered, "10 from bottom"]
}


DemoListWidget {
  type : ListWidget

  width : 500
  height : 700
  position : ["10", "centered"]

  listItemArchetype : DemoWidgets.DemoListItem
  listItemBinding : "listItems -> item"

  listItemGapSize : 0
}

DemoListItem {
  type : TextDisplayWidget
  text : "%(item.text)"

  drawBackground: true
  background.image: "ui/minimalistBorder_ne.png"
  fontScale : 2

  width : 100%
}