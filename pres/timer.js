$(function() {
  var start;
  reset();

  setInterval(redraw, 1000);
  $("#timer").click(reset);

  function redraw() {
    var time = (new Date()).getTime() / 1000 - start;

    var secs = Math.floor(time % 60);
    var mins = Math.floor(time / 60);

    var secsStr = secs >= 10 ? ""+secs : "0"+secs;
    var minsStr = ""+mins;

    $("#timer").text(minsStr + ":" + secsStr);
  }

  function reset() {
    start = (new Date()).getTime() / 1000;
    redraw();
  }
});
