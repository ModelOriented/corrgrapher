function showPlotOnSelect(properties){
  // Shows plot of variable, on which user clicked on graph
  // We have to retrieve base_id from the object, that called the function
  var obj = this.body.container.parentNode;
  while(!/^cgr_content_[0-9]{6}$/.test(obj.id)){
    obj = obj.parentElement;
  }
  var base_id = obj.id;
  var selected_node_label = this.body.data.nodes.get(properties.nodes[0]).label;
  showPlot(base_id, selected_node_label);
}

function showPlot(base_id, selected_node_label){
  // shows just the plot inside div of id base_id + '_' + selected_node_label
  // hides the rest of the plots inside div with base_id
  hidePlots(base_id);
  obj = document.getElementById(base_id + '_' + selected_node_label);
  obj.style.display = 'block';
  window.dispatchEvent(new Event('resize'));
}

function hidePlots(base_id){
  // hides all plots in tabcontent which are children of base_id
  var i, tabcontent;
  if(base_id === undefined){tabcontent = document.getElementsByClassName("cgr_tabcontent");}
  else {tabcontent = document.getElementById(base_id).getElementsByClassName("cgr_tabcontent");}
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = 'none';
  }
}
// hidePlots();

function addEventToSelect(properties){
  // retrieve base_id  
  var obj = this.body.container.parentNode;
  while(!/^cgr_content_[0-9]{6}$/.test(obj.id)){
    obj = obj.parentElement;
  }
  var base_id = obj.id;
  document.getElementById(base_id)
          .getElementsByTagName('SELECT').item(0)
          .addEventListener('change', function(event){
            // retrieve base_id
            var obj = this.parentNode;
            while(!/^cgr_content_[0-9]{6}$/.test(obj.id)){
              obj = obj.parentElement;
            }
            var base_id = obj.id;
            var selected_node_label = this.selectedOptions[0].text;
            showPlot(base_id, selected_node_label);
          });
}


