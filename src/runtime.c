#if defined(KOA_MAIN_RETURNS_I32)
int __koa_main(void);
#elif defined(KOA_MAIN_RETURNS_EMPTY)
void __koa_main(void);
#else
    #error "Unknown return type for koa_main"
#endif

int main(void)
{
    int ret;

#if defined(KOA_MAIN_RETURNS_I32)
    ret = __koa_main();
#elif defined(KOA_MAIN_RETURNS_EMPTY)
    __koa_main();
    ret = 0;
#endif
    return ret;
}
