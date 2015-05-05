from fbuild.builders.c import guess_static
import fbuild.db, os

@fbuild.db.caches
def copy(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    src.copy(dst)

def build(ctx):
    c = guess_static(ctx)
    c.compile('include-main.c', 'main.o')
    # Fbuild puts object files in ctx.buildroot,
    # which is good...except that Main.hs wants them in the current directory.
    # This symlinks it as a workaround
    try: os.symlink(ctx.buildroot / 'main.o', 'main.o')
    except: pass
