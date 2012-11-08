$(document).ready(function(){
  setInterval(refreshContent, 1000);
});

function refreshContent() {
  $.get("/erl/world_http:map", function(map) {
    asciimap = '';
    $.each(map.split('\n'), function(index, row) {
      asciimap += row.split('').join('  ') + '\n';
    });
    $('#map').html(asciimap);
  });
  $.get("/erl/world_http:options", function(options) {
    $('#options').html(options);
  });
}
