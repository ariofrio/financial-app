
var plaidLinkHandler = Plaid.create({
  clientName: 'Steward Money Management',
  env: 'sandbox',
  key: 'd7267ee308d740bab2bb0a3ff18fc9',
  product: ['transactions'],
  // webhook: 
  selectAccount: false,
  onSuccess: function(public_token, metadata) {
    app.ports.successes.send([public_token, metadata]);
  },
  onExit: function(err, metadata) {
    app.ports.exits.send([err, metadata]);
  },
});

app.ports.request.subscribe(function() {
  handler.open();
});
