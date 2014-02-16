Wishlist = Ember.Application.create({
  rootElement: "#wishlist"
});

Wishlist.ApplicationAdapter = DS.RESTAdapter.extend();

Wishlist.Router.map(function() {
  this.resource("wishes", { path: "/" });
});

Wishlist.WishesRoute = Ember.Route.extend({
  model: function() {
    return this.store.find("wish");
  }
});

Wishlist.WishesController = Ember.ArrayController.extend({
  actions: {
    createWish: function () {
      alert("Creating...");
    }
  }
});

Wishlist.Wish = DS.Model.extend({
  title: DS.attr("string"),
  description: DS.attr("string")
});


//TODO create input form


