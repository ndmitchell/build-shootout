from fbuild.target import register
from fbuild.path import Path
import fbuild.db

@fbuild.db.caches
def cp(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['cp', src, dst], 'cp')

for path in Path.glob('*.in'):
    out = path.replaceext('.out')
    @register(name=str(out))
    def func(ctx):
        cp(ctx, path, out)
