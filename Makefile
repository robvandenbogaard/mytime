build:
	elm make src/Main.elm --debug --output index.html

serve:
	elm reactor --port=8011
