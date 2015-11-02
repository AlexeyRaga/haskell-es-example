# haskell-es

An example of DDD + EventSourcing in Haskell.

### To run the example

- Download and run the EventStore (https://geteventstore.com/)
  - Navigate to http://127.0.0.1:2113/ (default credentials are `admin/changeit`) to ensure you have ES running
- `git clone https://github.com/AlexeyRaga/haskell-es-example.git`
- `cd haskell-es-example`
- `stack build`
- `./haskell-es-exe`

The following commands are accepted:

    data Command Card = AddProduct ProductId
                      | RemoveProduct ProductId
                      | ClearCard
                      deriving (Show, Read)

The commands type derives `Read` (for example purposes only) so it can be used in the console as:

    AddProduct (ProductId "prod-1")
    RemoveProduct (ProductId "prod-1")
    ClearCard

Enter `exit` to leave the session.
Enter `expose` to show the current card state.

A session could look like:

    Your current card is:
    Card {cardId = CardId "test-card-1", products = []}

    Enter your commands here:
    AddProduct (ProductId "prod-1")
    --> ProductAdded {addedProductId = ProductId "prod-1"}
    AddProduct (ProductId "prod-2")
    --> ProductAdded {addedProductId = ProductId "prod-2"}
    AddProduct (ProductId "prod-1")
    --> ProductAdded {addedProductId = ProductId "prod-1"}
    AddProduct (ProductId "prod-1")
    --! QuantityExceedsLimit (ProductId "prod-1") 2
    RemoveProduct (ProductId "prod-1")
    --> ProductRemoved {removedProductId = ProductId "prod-1"}
    RemoveProduct (ProductId "prod-3")
    --! NoProductInCard (ProductId "prod-3")
    expose
    Card {cardId = CardId "test-card-1", products = [CardItem {productId = ProductId "prod-1", quantity = 1},CardItem {productId = ProductId "prod-2", quantity = 1}]}
    exit
    Bye.

All the events are stored in the EventStore therefore the state of the aggregate is persisted between runs.

Go to the "Stream Browser" menu of the EventStore UI (http://127.0.0.1:2113/web/index.html#/streams) to see the streams. This example uses `test-card-1` stream. Navigating to this stream will show all the history of the aggregate (all the events), which is one of the biggest points.
