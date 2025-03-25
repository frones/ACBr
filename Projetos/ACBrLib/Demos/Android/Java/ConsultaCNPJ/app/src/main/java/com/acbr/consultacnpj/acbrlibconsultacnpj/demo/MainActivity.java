package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;
import android.util.Log;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.navigation.NavController;
import androidx.navigation.fragment.NavHostFragment;
import androidx.navigation.ui.AppBarConfiguration;
import androidx.navigation.ui.NavigationUI;
import com.google.android.material.bottomnavigation.BottomNavigationView;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class MainActivity extends AppCompatActivity {
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    private ConsultaCNPJApplication application;
    private NavController navController;
    private AppBarConfiguration appBarConfiguration;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        application = ((ConsultaCNPJApplication) getApplicationContext());
        ACBrConsultaCNPJ = application.getAcBrLibConsultaCNPJ();
        configurarACBrConsultaCNPJ();

        // Configurar Toolbar
        Toolbar toolbar = findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        // Configurar navegação
        NavHostFragment navHostFragment = (NavHostFragment) getSupportFragmentManager()
                .findFragmentById(R.id.nav_host_fragment);
        navController = navHostFragment.getNavController();
        
        // Configurar AppBar
        appBarConfiguration = new AppBarConfiguration.Builder(
                R.id.navigation_comandos,
                R.id.navigation_configuracoes,
                R.id.navigation_ini
        ).build();
        
        NavigationUI.setupActionBarWithNavController(this, navController, appBarConfiguration);
        
        // Configurar BottomNavigationView
        BottomNavigationView bottomNavigationView = findViewById(R.id.bottom_navigation);
        NavigationUI.setupWithNavController(bottomNavigationView, navController);
    }

    @Override
    public boolean onSupportNavigateUp() {
        return NavigationUI.navigateUp(navController, appBarConfiguration)
                || super.onSupportNavigateUp();
    }

    private void configurarACBrConsultaCNPJ() {
        try {
            ACBrConsultaCNPJ.inicializar();
            aplicarConfiguracoesPadrao();
        } catch (Exception e) {
            Log.e("ACBrLibConsultaCNPJ", "Erro ao inicializar ACBrLibConsultaCNPJ", e);
        }
    }

    private void aplicarConfiguracoesPadrao() {
        try {
            ACBrConsultaCNPJ.configGravarValor("Principal", "LogPath", application.getLogPath());
            ACBrConsultaCNPJ.configGravarValor("Principal", "LogNivel", "4");
            ACBrConsultaCNPJ.configGravar();
            ACBrConsultaCNPJ.configLer(application.getArqConfigPath());
        } catch (Exception e) {
            Log.e("MainActivity", e.getMessage());
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ACBrConsultaCNPJ.finalizar();
    }
}