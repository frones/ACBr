using System;
using System.ComponentModel;

namespace ACBrLib
{
    public interface ISplash
    {
        void ShowInfo(SplashInfo tipo, params object[] args);
    }
}