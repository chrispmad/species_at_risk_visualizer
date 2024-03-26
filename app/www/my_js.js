// Disable click events on the species-at-risk layers
// I'll have to rethink this if people want tables of information when clicked...
Shiny.addCustomMessageHandler('disableLayerBClickPropagation', function() {
  map.eachLayer(function(layer) {
    if (layer.options.id === 'cdc_aquatic_sar_layer' | layer.options.id === 'dfo_sara_layer' | layer.options.id === 'critical_habitat_layer') {
      layer.off('click');
    }
  });
});
