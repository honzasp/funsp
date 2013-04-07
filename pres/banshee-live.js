$(function() {
  var slices = [
    ["&X..." ,"XX.@." ,".X.X." ,"@..X@" ,"..X#." ],
    ["&X.@." ,"XX..." ,".X.X@" ,".@.X." ,"..X#." ],
    ["&X..@" ,"XX..@" ,".X.X." ,"..@X." ,"..X#." ],
    ["&X..." ,"XX.@@" ,".X.X." ,".@.X." ,"..X#." ],
    ["&X..." ,"XX.@@" ,".X.X." ,"@..X." ,"..X#." ],
    ["&X.@." ,"XX..." ,".X.X@" ,".@.X." ,"..X#." ],
    ["&X..@" ,"XX..." ,".X.X." ,"..@X@" ,"..X#." ],
    ["&X..." ,"XX..@" ,".X.X@" ,".@.X." ,"..X#." ],
    ["&X..." ,"XX.@@" ,".X.X." ,"@..X." ,"..X#." ],
    ["&X.@." ,"XX.@." ,".X.X." ,".@.X." ,"..X#." ],
    ["&X..@" ,"XX..@" ,".X.X." ,"..@X." ,"..X#." ],
    ["&X..." ,"XX..@" ,".X.X@" ,".@.X." ,"..X#." ]
  ];

  var step = 0;

  $("#banshee-castle .back").click(function() {
    if(step > 0) {
      step = step - 1;
    } else {
      step = slices.length - 1;
    }
    redraw();
  });

  $("#banshee-castle .forward").click(function() {
    if(step >= slices.length) {
      step = 0;
    } else {
      step = step + 1;
    }
    redraw();
  });

  redraw();

  function redraw() {
    var castle = $("#banshee-castle .castle");
    var slice = slices[step];

    castle.html(slice.join("<br>"));
  }
});
