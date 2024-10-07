package com.acbr.nfe.acbrlibnfe.demo;


import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ConfiguracoesActivity extends AppCompatActivity {

    private EditText editExportedConfig;
    //Propriedades do ACBrLibNFe
    private ACBrLibNFe ACBrNFe;

    private NfeApplication application;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_configuracoes);

        Button btnAbrirConfigNFe = findViewById(R.id.btnAbrirConfigNFe);

        btnAbrirConfigNFe.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view) {
                irParaTelaConfiguracoesNFe();
            }
        });

        this.editExportedConfig = findViewById(R.id.editExportedConfig);

        this.application = (NfeApplication)getApplicationContext();

        this.ACBrNFe = application.getAcBrLibNFe();

    }

    private void irParaTelaConfiguracoesNFe() {
        Intent intent = new Intent(ConfiguracoesActivity.this, ConfiguracoesNFeActivity.class);
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
            result = ACBrNFe.configExportar();
            editExportedConfig.setText(result);
        } catch (Exception e) {
            Log.e("verConfiguracoesAtuais", "Erro ao exportar configurações", e);
            result = e.getMessage();
        } finally {
            editExportedConfig.setText(result);
        }
    }
}
