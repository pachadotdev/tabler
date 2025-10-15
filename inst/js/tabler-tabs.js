// Tab switching functionality for Tabler dashboard
$(document).ready(function() {
  // Handle menu item clicks for tab switching
  $(document).on('click', 'a[data-toggle="tab"]', function(e) {
    e.preventDefault();
    
    var targetId = $(this).attr('data-target');
    
    if (!targetId) return;
    
    // Remove active class from all nav links
    $('a[data-toggle="tab"]').removeClass('active');
    
    // Add active class to clicked link
    $(this).addClass('active');
    
    // Hide all tab panes
    $('.tab-pane').removeClass('show active');
    
    // Show target tab pane
    $(targetId).addClass('show active');
    
    // Trigger window resize to ensure charts render properly
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
      
      // Trigger Shiny resize for outputs in the tab
      if (window.Shiny) {
        $(targetId).find('.shiny-bound-output').trigger('shown');
      }
    }, 50);
    
    // Update Shiny input if available
    if (window.Shiny) {
      var tabName = targetId.replace('#', '');
      Shiny.setInputValue('activeTab', tabName, {priority: 'event'});
    }
  });
  
  // Activate first tab by default if none are active
  setTimeout(function() {
    if ($('.tab-pane.active').length === 0) {
      var firstTab = $('.tab-pane').first();
      if (firstTab.length > 0) {
        var tabId = firstTab.attr('id');
        firstTab.addClass('show active');
        
        // Activate corresponding menu item
        $('a[data-target="#' + tabId + '"]').addClass('active');
        
        // Trigger resize for initial tab
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 100);
      }
    }
  }, 100);
  
  // Handle Shiny outputs becoming visible
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('tabler-show-tab', function(message) {
      var targetId = '#' + message.tab;
      $('a[data-target="' + targetId + '"]').trigger('click');
    });
  }
});
