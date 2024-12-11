package com.example.demoacbrbal;


import java.io.File;

import io.flutter.app.FlutterApplication;

public class DemoBalApplication extends FlutterApplication {
    private File appDir;
    @Override
    public void onCreate() {
        super.onCreate();
        appDir = getExternalFilesDir(null);
        if (!appDir.exists()) {
            appDir.mkdirs();
        }
    }

    public File getAppDir() {
        return appDir;
    }
}