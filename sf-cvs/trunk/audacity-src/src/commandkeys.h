// BG: Generate an array of keys combos that cannot be used
// BG: Included inside of Menu.cpp

#define CMD_CKAKEY(a) ( (wxKeyEvent *)memcpy(malloc(sizeof(wxKeyEvent)), &a, sizeof(wxKeyEvent)) )

wxKeyEvent tmpKeyEvent;

tmpKeyEvent.m_x = 0;
tmpKeyEvent.m_y = 0;
tmpKeyEvent.m_keyCode = (wxKeyCode)WXK_PRIOR;
tmpKeyEvent.m_controlDown = false;
tmpKeyEvent.m_shiftDown = false;
tmpKeyEvent.m_altDown = false;
tmpKeyEvent.m_metaDown = false;
tmpKeyEvent.m_scanCode = false;

mCommandUsedKey.Add( CMD_CKAKEY(tmpKeyEvent) );
