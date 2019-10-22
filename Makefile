clean:
	find $(CURDIR) -type d -name ".spago" -depth 2 -exec rm -rf {} \;
	find $(CURDIR) -type d -name "output" -depth 2 -exec rm -rf {} \;
