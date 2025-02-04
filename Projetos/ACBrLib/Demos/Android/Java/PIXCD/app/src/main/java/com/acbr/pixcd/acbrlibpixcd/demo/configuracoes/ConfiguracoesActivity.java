package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes;

import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import androidx.appcompat.app.AppCompatActivity;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesActivity extends AppCompatActivity {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;
    private EditText editExportedConfig;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_configuracoes);

        Button btnAbrirConfigPIXCD = findViewById(R.id.btnAbrirConfigPIXCD);
        btnAbrirConfigPIXCD.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View view) {
                irParaTelaConfiguracoesPIXCD();
            }
        });

        this.editExportedConfig = findViewById(R.id.editExportedConfig);

        this.application = (PIXCDApplication) getApplicationContext();

        this.ACBrPIXCD = application.getACBrLibPIXCD();

        verConfiguracoesAtuais();
    }

    private void irParaTelaConfiguracoesPIXCD(){
        Intent intent = new Intent(ConfiguracoesActivity.this, ConfiguracoesPIXCDActivity.class);
        startActivity(intent);
    }

    public void onClickButtonExportarConfig(View view) throws InterruptedException {
        try {
            verConfiguracoesAtuais();
        } catch (Exception ex) {
            Log.e("onClickButtonExportarConfig", "Erro ao visualizar as configurações", ex);
        }
    }

    public void verConfiguracoesAtuais(){
        String config = editExportedConfig.getText().toString();
        String result = "";
        try{
            Log.i("verConfiguracoesAtuais", config);
            result = ACBrPIXCD.configExportar();
            editExportedConfig.setText(result);
        } catch (Exception ex){
            Log.e("verConfiguracoesAtuais", "Erro ao exportar configurações", ex);
            result = ex.getMessage();
        } finally{
            editExportedConfig.setText(result);
        }
    }
}