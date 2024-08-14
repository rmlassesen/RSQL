; Main environment for testing

(load "sqlists.lsp")



(defun main () 

	(write-table-form
		"products"
		'("id" "produkt_navn" "produkt_pris" "produkt_beskrivelse")
		'("id")
		'(0 4 2 4)) ;Data-types (Pos)Integer, String, (Signed)Integer, String


	(write-row	"products" 
			("produkt_navn" "produkt_pris" "produkt_beskrivelse")
			("Robot" 1989 "En fin robot"))

)