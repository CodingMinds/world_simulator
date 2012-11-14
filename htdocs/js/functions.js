var activePid;

$(document).ready(function(){
  setInterval(refreshContent, 1000);
  setInterval(refreshWorldListing, 10000);
  refreshWorldListing();
});

function activate(Pid) {
  activePid = Pid;
  refreshContent();
  refreshWorldListing();
}

function refreshWorldListing() {
  $.getJSON('/erl/world_http:worlds', function(worlds) {
    var items = [];

    $.each(worlds, function(key, val) {
      if(!activePid) {
        activePid = val.Pid;
      }
      
      items.push('<li' + (val.Pid == activePid ? ' class="active"' : '')
        + '><a href="javascript:activate(\'' + val.Pid + '\');">'
        + val.Name + ' (' + val.X + 'x' + val.Y + ') ' + val.AgentCount
        + ' of ' + val.MaxAgents + ' agents online</a></li>');
    });

    $('#worlds_list').replaceWith($('<ul/>', {
      'id': 'worlds_list',
      'class': 'nav nav-pills nav-stacked',
      html: items.join('')
    }));
  });
}

function refreshContent() {
  if(!activePid) {
    return;
  }
  
  $.get("/erl/world_http:map?" + activePid, function(map) {
    asciimap = '';
    $.each(map.split('\n'), function(index, row) {
      asciimap += row.split('').join('  ') + '\n';
    });
    $('#map').html(asciimap);
  });
  
  $.get("/erl/world_http:options?" + activePid, function(options) {
    $('#options').html(options);
  });
}
