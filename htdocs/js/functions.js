$(document).ready(function(){
  setInterval(refreshContent, 1000);
});

function refreshContent() {
  $.get("/erl/world_http:map", function(map) {
    $('#map').html(map);
  });
  $.get("/erl/world_http:options", function(options) {
    $('#options').html(options);
  });
}
