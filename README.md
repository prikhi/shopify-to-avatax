# Shopify -> Avalara AvaTax Converter

A haskell program that converts a Shopify Order Export CSV into an XLSX for
AvaTax Transaction Imports.

1. From your Shopify admin, go to the Orders page.
1. Filter this however you'd like(e.g., only last month), then click the Export
   button.
1. Wait for the order export email to arrive & download it.
1. Copy the order export to this directory, renaming it to `orders_export_1.csv`.
1. Run the script using `stack run`.
1. Open the generated `avatax-wholesale-transactions.csv` in Excel/LibreOffice.
1. Review the generated fields & perform any necessary cleanup.
1. Copy each generated column into the appropriate column of AvaTax's
   `Transaction Import` template spreadsheet.
1. Import the template spreadsheet from `Transactions > Import Transactions` on
   the Avalara website.


There's some hardcoded stuff here that you might want to switch out if you use
it yourself:

* Input/output file names from `app/Main.hs`
* Shipping item code/name.
* Origin region & postal code.
* Prefixes for the customer ID & order ID.

Pull requests to abstract those out are welcome.


## LICENSE

GPL-3.0+
