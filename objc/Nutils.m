#import <Cocoa/Cocoa.h>
#import <Nu/Nu.h>
#import <objc/objc-class.h>

void NutilsInit()
{
    static initialized = 0;
    if (!initialized) {
        initialized = 1;
        //[Nu loadNuFile:@"cl_utils" fromBundleWithIdentifier:@"nu.programming.Nutils" withContext:nil];
        //[Nu loadNuFile:@"range" fromBundleWithIdentifier:@"nu.programming.Nutils" withContext:nil];
        //[Nu loadNuFile:@"with_test" fromBundleWithIdentifier:@"nu.programming.Nutils" withContext:nil];
        //[Nu loadNuFile:@"with_object" fromBundleWithIdentifier:@"nu.programming.Nutils" withContext:nil];
    }
}

@interface Nutils : NSObject

@end

@implementation Nutils

+ (void) load
{
	NutilsInit();
}

+ (id) objectWithAddress:(unsigned long)address
{
	id object = (id)address;
	return object;
}

+ (long) addressOfObject:(id) obj
{
	return (long)obj;
}

+ (NSString*) ivarLayoutForClass:(NSString*) className ivar:(NSString*) ivarName
{
	Class	cls;
	
	const char* szClassName = [className UTF8String];
	cls = objc_getClass(szClassName);
	const char* szIvarLayout = class_getIvarLayout(cls);
	NSString* layout = [[NSString alloc] initWithUTF8String:szIvarLayout];
	return layout;
}


+ (NSMutableArray*) ivarsForClass: (NSString*) className
{
	Ivar *ivarList = NULL;
	unsigned int ivarListCount;
	
	const char* szClassName = [className UTF8String];
	Class cls = objc_getClass(szClassName);
	ivarList = class_copyIvarList(cls, &ivarListCount);

	NSMutableArray* arr = [[NSMutableArray alloc] init];

	int i;
	for (i = 0; i < ivarListCount; i++)
	{
		NSString* name = [NSString stringWithCString:ivar_getName(ivarList[i])];
		[arr addObject:name];
	}

	free(ivarList);

	return arr;
}

@end
