// Used at the initialization to hide all plots. Soon after that first plot is shown

function hidePlots(){
  var i, tabcontent;
  tabcontent = document.getElementsByClassName("cgr_tabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = 'none';
  }
}
hidePlots();
function addEventToSelect(base_id){
  console.log(document.getElementById(base_id)
                      .getElementsByTagName('SELECT').item(0));
  document.getElementById(base_id)
          .getElementsByTagName('SELECT').item(0)
          .addEventListener('change', function(event){
            var selected_node_label = this.selectedOptions[0].text;
            hidePlots();
            obj = document.getElementById(base_id + '_' + selected_node_label);
            obj.style.display = 'block';
            window.dispatchEvent(new Event('resize'));
      });
}



