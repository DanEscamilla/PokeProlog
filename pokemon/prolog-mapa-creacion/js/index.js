columns = 10;
rows = 10;
elementWidth = 30;
elementHeight = 25;
walls = {};
container = $('#container');
dragging = false;
button = null;
tileMap = null;
widthInput = $('#width-input').eq(0);
heightInput = $('#height-input').eq(0);
specialNameInput = $('#special-name').eq(0);
specialDisplayInput = $('#special-display').eq(0);


function createRow(rowNumber,columns){
  let row = [];
  for (let i=0;i<columns;i++){
    div = $('<div />', {
        "class": 'item',
        css:{
          width: elementWidth-2,
          height: elementHeight-2
        },
        ondrag:(e)=>{return false},
        mousedown:(e)=>{
          e.preventDefault ? e.preventDefault() : e.returnValue = false;
          button = e.which;
          console.log(button);
          dragging=true;
          handleDrag(i,rowNumber);
        },
        mouseup:(e)=>{
          button = e.which;
          dragging=false
        },
        mouseenter: handleDrag.bind(null,i,rowNumber)
    });
    container.append(div);
    row[i] = div;
  }
  return row;
}

function handleDrag(columnNumber,rowNumber){
  if (dragging){
    switch(button){
      case 1:
        addCell(columnNumber,rowNumber);
        break;
      case 2:
        addSpecialCell(columnNumber,rowNumber);
        break;
      case 3:
        removeCell(columnNumber,rowNumber);
        break;
    }
  }
}

function countWallType(columnNumber,rowNumber){
  let wall = walls[cantorPairingFn(columnNumber,rowNumber)];
  if (wall){
    return 1;
  } else
    return 0;
}

function determineWallType(columnNumber,rowNumber){
  determineWallTypeNoDepth(columnNumber,rowNumber);
  determineWallTypeNoDepth(columnNumber+1,rowNumber);
  determineWallTypeNoDepth(columnNumber-1,rowNumber);
  determineWallTypeNoDepth(columnNumber,rowNumber+1);
  determineWallTypeNoDepth(columnNumber,rowNumber-1);
}
function determineWallTypeNoDepth(columnNumber,rowNumber){
  if (!walls[cantorPairingFn(columnNumber,rowNumber)]){
    return;
  }
  horizontal = countWallType(columnNumber-1,rowNumber) +
    countWallType(columnNumber+1,rowNumber);
  vertical = countWallType(columnNumber,rowNumber-1) +
    countWallType(columnNumber,rowNumber+1);
  if (vertical>horizontal){
      tileMap[rowNumber][columnNumber].css({
        backgroundColor: 'blue'
      });
    walls[cantorPairingFn(columnNumber,rowNumber)].display = " | ";
  } else {
      tileMap[rowNumber][columnNumber].css({
        backgroundColor: 'black'
      });
    walls[cantorPairingFn(columnNumber,rowNumber)].display =  "---";
  }

}

function addCell(columnNumber,rowNumber){
  let index = cantorPairingFn(columnNumber,rowNumber);
  walls[index] = {
    name:'wall',
    x:columnNumber,
    y:rowNumber
  }
  determineWallType(columnNumber,rowNumber);
}


function addSpecialCell(columnNumber,rowNumber){
  let index = cantorPairingFn(columnNumber,rowNumber);
  walls[index] = {
    name:specialNameInput.val(),
    display:specialDisplayInput.val().padEnd(3, ' '),
    x:columnNumber,
    y:rowNumber
  }
  tileMap[rowNumber][columnNumber].css({
    backgroundColor: 'orange'
  });
}

function removeCell(columnNumber,rowNumber){
  tileMap[rowNumber][columnNumber].css({
    backgroundColor: 'transparent'
  });
  delete walls[cantorPairingFn(columnNumber,rowNumber)];
}


function buildTileMap(rows,columns){
  tileMap = [];
  walls = {};
  container.empty();

  container.css({
    width:(elementWidth*(columns))+"px",
    height:(elementHeight*(rows))+"px"
  })
  for (let i=0;i<rows;i++){
    tileMap[i] = createRow(i,columns);
  }
}

function cantorPairingFn(a,b){
  return 12*(a+b)*(a+b+1)+b;
}

function generatePrologList(){
  let str = "[\n";
  for (var property in walls) {
    if (walls.hasOwnProperty(property)) {

      display = (walls[property].name == "wall")?"|||":walls[property].display;

       str += "[\""+
         display + "\"," +
         (walls[property].x+1) + "," +
         (walls[property].y+1) + ",\"" +
         walls[property].name + "\"],\n";
    }
  }
  str += "\n]";
  return str;
}

function handleTileMapCreation(){
  let width = parseInt(widthInput.val());
  let height = parseInt(heightInput.val());
  columns = width;
  rows = height;

  buildTileMap(rows,columns);
}

function handleListCreation(){
  console.log(generatePrologList());
}
