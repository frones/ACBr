package br.com.acbr.lib.demo.posprinter.demo_acbrlib_posprinter;

import androidx.annotation.NonNull;

import io.flutter.embedding.android.FlutterActivity;
import io.flutter.embedding.engine.FlutterEngine;
import io.flutter.plugins.GeneratedPluginRegistrant;

public class MainActivity extends FlutterActivity {
    @Override
    public void configureFlutterEngine(@NonNull FlutterEngine flutterEngine) {
        super.configureFlutterEngine(flutterEngine);
       // flutterEngine.getPlugins().add(new ACBrLibPosPrinterPlugin());
    }
}
