from fbuild.target import register
import fbuild.db

@fbuild.db.caches
def gen(ctx, src: fbuild.db.SRC, dsts: fbuild.db.DSTS):
    ctx.execute(['sh', 'multiple-gen', src, '--'] + dsts, 'multiple-gen')

@fbuild.db.caches
def run(ctx, src: fbuild.db.SRC, dst: fbuild.db.DST):
    ctx.execute(['sh', 'multiple-run', src, '--', dst], 'multiple-run')

@register()
def output1(ctx):
    gen(ctx, 'input', ['source1', 'source2'])
    run(ctx, 'source1', 'output1')

@register()
def output2(ctx):
    gen(ctx, 'input', ['source1', 'source2'])
    run(ctx, 'source2', 'output2')
