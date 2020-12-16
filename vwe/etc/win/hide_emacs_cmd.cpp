#include <windows.h>

int main()
{
	HWND hwnd = NULL;
	WCHAR name[10] = {'e', 'm', 'a', 'c', 's', '\0'};

	if (!hwnd)
	{
		hwnd = FindWindow(NULL, name);
		ShowWindow(hwnd, SW_HIDE);
	}

	// system("pause");
	return 0;
}
