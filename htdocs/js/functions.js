var activePid;

$(document).ready(function(){
  setInterval(refreshContent, 1000);
  setInterval(refreshWorldListing, 10000);
  setInterval(refreshPidListing, 10000);
  refreshWorldListing();
  refreshPidListing();
});

function activate(Pid) {
  activePid = Pid;
  refreshContent();
  refreshWorldListing();
  refreshPidListing();
}

function refreshWorldListing() {
  $.getJSON('/erl/world_http:worlds', function(worlds) {
    var items = [];
    var activePidValidated = false;
    var firstPid;
    
    $.each(worlds, function(key, val) {
      if(!firstPid)
        firstPid = val.Pid;
      
      if(val.Pid == activePid) {
        activePidValidated = true;
        return false;
      }
    });
    
    if(false == activePidValidated) {
      activePid = firstPid;
    }
    
    $.each(worlds, function(key, val) {
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
    $('#map').html(map);
  });
  
  $.get("/erl/world_http:options?" + activePid, function(options) {
    $('#options').html(options);
  });
}

function refreshPidListing() {
  if(!activePid) {
    return;
  }
  
  $.get("/erl/world_http:pids?" + activePid, function(pids) {
    $('#pid_mapping').html(pids);
  });
}
