package com.acbr.cep.acbrlibcep.demo;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import br.com.acbr.lib.cep.ACBrLibCep;

public class ConfiguracoesActivity extends AppCompatActivity {

    private EditText editExportedConfig;
    private ACBrLibCep ACBrCEP;
    private CEPApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_configuracoes);

        Button btnAbrirConfigCEP = findViewById(R.id.btnAbrirConfigCEP);

        btnAbrirConfigCEP.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view) {
                irParaTelaConfiguracoesCEP();
            }
        });

        this.editExportedConfig = findViewById(R.id.editExportedConfig);

        this.application = (CEPApplication)getApplicationContext();

        this.ACBrCEP = application.getAcBrLibCEP();

    }

    private void irParaTelaConfiguracoesCEP() {
        Intent intent = new Intent(ConfiguracoesActivity.this, ConfiguracoesCEPActivity.class);
        startActivity(intent);
    }

    public void onClickButtonExportarConfig(View view) throws InterruptedException {
        try {
            verConfiguracoesAtuais();
        } catch (Exception ex) {
            Log.e("onClickButtonExportarConfig", "Erro ao visualizar as configurações", ex);
        }
    }

    public void verConfiguracoesAtuais() {
        String config = editExportedConfig.getText().toString();
        String result = "";
        try {
            Log.i("verConfiguracoesAtuais", config);
            result = ACBrCEP.configExportar();
            editExportedConfig.setText(result);
        } catch (Exception e) {
            Log.e("verConfiguracoesAtuais", "Erro ao exportar configurações", e);
            result = e.getMessage();
        } finally {
            editExportedConfig.setText(result);
        }
    }
}