// Load and display last updated timestamp
document.addEventListener('DOMContentLoaded', function() {
  // Determine the root path based on current location
  const path = window.location.pathname;
  const depth = (path.match(/\//g) || []).length - 1;
  const rootPath = depth > 1 ? '../'.repeat(depth - 1) : '';
  
  // Load last updated timestamp
  fetch(rootPath + 'last-updated.json')
    .then(response => response.json())
    .then(data => {
      const lastUpdatedElement = document.getElementById('last-updated');
      if (lastUpdatedElement) {
        lastUpdatedElement.textContent = data.last_updated;
      }
    })
    .catch(error => {
      console.log('Last updated timestamp not available');
      const lastUpdatedElement = document.getElementById('last-updated');
      if (lastUpdatedElement) {
        lastUpdatedElement.textContent = '';
      }
    });
});
