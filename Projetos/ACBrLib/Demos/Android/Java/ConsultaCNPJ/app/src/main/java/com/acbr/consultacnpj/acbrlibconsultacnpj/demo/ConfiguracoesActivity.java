package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

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

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ConfiguracoesActivity extends AppCompatActivity {

    private EditText editExportedConfig;
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    private ConsultaCNPJApplication application;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_configuracoes);

        Button btnAbrirConfigConsultaCNPJ = findViewById(R.id.btnAbrirConfigConsultaCNPJ);

        btnAbrirConfigConsultaCNPJ.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view) {
                irParaTelaConfiguracoesConsultaCNPJ();
            }
        });

        this.editExportedConfig = findViewById(R.id.editExportedConfig);

        this.application = (ConsultaCNPJApplication)getApplicationContext();

        this.ACBrConsultaCNPJ = application.getAcBrLibConsultaCNPJ();

    }

    private void irParaTelaConfiguracoesConsultaCNPJ() {
        Intent intent = new Intent(ConfiguracoesActivity.this, ConfiguracoesConsultaCNPJActivity.class);
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
            result = ACBrConsultaCNPJ.configExportar();
            editExportedConfig.setText(result);
        } catch (Exception e) {
            Log.e("verConfiguracoesAtuais", "Erro ao exportar configurações", e);
            result = e.getMessage();
        } finally {
            editExportedConfig.setText(result);
        }
    }
}