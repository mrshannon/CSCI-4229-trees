# Commands:
#	make			= builds program statically
#	make static		= builds program statically
#	make dynamic	= builds program dynamically
#	make all		= builds everything (builds program statically)
#	make clean		= cleans up the build but does not remove the sandbox
#	make clean-tmp	= same as clean but does not remove the executable
#	make clean-all	= same as clean but also removes the sandbox


### Configuration
################################################################################

# Program Name
PROGRAM=hw07

# Sources
SOURCES=src/Time.hs \
        src/Math/OpenGL.hs \
        src/Math/Vector/Types.hs \
        src/Math.hs \
        src/App/Types.hs \
        src/App/GLUT.hs \
        src/Main.hs \
        src/Error.hs \
        src/View/Types.hs \
        src/World/Types.hs \
        src/Settings.hs \
        src/App.hs \
        src/View.hs \
        src/Logic/Controls.hs \
        src/Logic/Controls/Animate.hs \
        src/Logic/Controls/Zoom.hs \
        src/Logic/Controls/Move.hs \
        src/Logic/Controls/Rotate.hs \
        src/Logic/Animation.hs \
        src/Settings/Types.hs \
        src/Time/Types.hs \
        src/Time/GLUT.hs \
        src/Window.hs \
        src/Graphics/Objects/PineTree.hs \
        src/Graphics/OpenGL/Help.hs \
        src/Graphics/Primitives/Open.hs \
        src/Graphics/Primitives/Closed.hs \
        src/Graphics/Primitives/Flat.hs \
        src/Graphics/Lighting.hs \
        src/Graphics/Texture.hs \
        src/Graphics/Skybox.hs \
        src/Graphics/OpenGL.hs \
        src/Graphics/Rendering/OpenGL/Draw.hs \
        src/Graphics/Rendering/OpenGL/Help.hs \
        src/Graphics/Types.hs \
        src/Graphics/Text.hs \
        src/Graphics/Status.hs \
        src/Window/Types.hs \
        src/Window/GLUT.hs \
        src/Graphics.hs \
        src/World.hs \
        src/Input/Mouse/GLUT.hs \
        src/Input/Mouse/Types.hs \
        src/Input/Mouse.hs \
        src/Input/Keyboard/GLUT.hs \
        src/Input/Keyboard/Types.hs \
        src/Input/Keyboard.hs

################################################################################


### Internal Variables
################################################################################
static: GHCFLAGS=-O2
dynamic: GHCFLAGS=-O2 -dynamic

export CABAL_SANDBOX_CONFIG=$(PWD)/cabal.sandbox.config
################################################################################


### Rules
################################################################################

.PHONY: all
all: static

# Make a static binary.
.PHONY: static
static: sandbox $(PROGRAM)

# Make a dynamic binary.
.PHONY: dynamic
dynamic: sandbox $(PROGRAM)

# Install a cabal sandbox.
.PHONY: sandbox
sandbox: .cabal-sandbox

# Clean up the build.
.PHONY: clean 
clean: clean-tmp
	rm -f $(PROGRAM)

# Clean up the intermediate build files.
.PHONY: clean-tmp
clean-tmp:
	cabal clean

# Clean up all files including the binary and sandbox.
.PHONY: clean-all
clean-all: clean clean-sandbox

################################################################################


### Internal Rules
################################################################################

.cabal-sandbox:
	cabal sandbox init
	cabal --require-sandbox install --jobs --only-dependencies

.PHONY: clean-sandbox
clean-sandbox:
	cabal sandbox delete

dist:
	cabal --require-sandbox install --jobs --only-dependencies
	cabal configure --ghc-options="$(GHCFLAGS)"

$(PROGRAM): dist/build/$(PROGRAM)/$(PROGRAM)
	cp dist/build/$(PROGRAM)/$(PROGRAM) $(PROGRAM)

dist/build/$(PROGRAM)/$(PROGRAM): $(SOURCES) dist
	cabal --require-sandbox install --jobs --only-dependencies
	cabal build

################################################################################
