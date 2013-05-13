$(function() {
  var round = 0;
  var round_count = 3;

  refresh();

  $("#ban-rounds .backward").click(function() {
    if(round > 0) {
      round = round - 1;
      refresh();
    }
  });

  $("#ban-rounds .forward").click(function() {
    if(round < round_count - 1) {
      round = round + 1;
      refresh();
    }
  });

  function refresh() {
    $("#ban-rounds .screen img").removeClass("active");
    $("#ban-rounds .screen .r" + round).addClass("active");
  }
});
