$(function() {
  var fibs = [1,1,2,3,5,8,13,21,34,55,89,144,233,377];
  var step = 0;
  var maxStep = fibs.length - 3;

  function updateHTML() {
    updateList($("#fibs-live .list code"), fibs.slice(0,step+3));
    updateList($("#fibs-live .zip-top code"), fibs.slice(0,step+3));
    updateList($("#fibs-live .zip-bottom code"), fibs.slice(1,step+3));
  }

  function updateList(listElem, nums) {
    listElem.empty();
    $("<span class='bracket'>[</code>").appendTo(listElem);
    nums.forEach(function(num) {
      var numElem = $("<span />");
      numElem.addClass("num");
      numElem.text(num+",");
      numElem.appendTo(listElem);
    });
    $("<div class='clear'>.</div>").appendTo(listElem);
  }

  updateHTML();

  $("#fibs-live .back").click(function() {
    if(step > 0) {
      step = step - 1;
      $("#fibs-live").removeClass("going-forward");
      $("#fibs-live").addClass("going-back");
      setTimeout(function() {
        $("#fibs-live").removeClass("going-back");
        updateHTML();
      }, 1000);
    }
  });

  $("#fibs-live .forward").click(function() {
    if(step < maxStep) {
      $("#fibs-live").addClass("going-forward");
      step = step + 1;
      updateHTML();
    }
  });
});
