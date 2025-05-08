package br.com.acbr.lib.posprinter.posprinter;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import java.io.File;

import br.com.acbr.lib.posprinter.ACBrLibPosPrinter;

public class MainActivity extends AppCompatActivity {

    private Button buttonToggleActivate;
    private Button buttonPrint ;
    private Button buttonCutPaper;
    private Button buttonShowIni;

    private ACBrLibPosPrinter posPrinter;

    private String port = "";
    private String arquivoConfig = "";

    private String memoryFileConfig ="";

    private File appDir;
    private Toolbar toolbar;

    private EditText editLog;

    private boolean isActivate  = false;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        setContentView(R.layout.activity_main);
        ViewCompat.setOnApplyWindowInsetsListener(findViewById(R.id.main), (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemBars.left, systemBars.top, systemBars.right, systemBars.bottom);
            return insets;
        });

        toolbar = findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);


        buttonToggleActivate = findViewById(R.id.buttonToggleActivate);
        buttonPrint = findViewById(R.id.buttonPrint);
        buttonCutPaper = findViewById(R.id.buttonCutPaper);
        buttonShowIni = findViewById(R.id.buttonIni);

        buttonToggleActivate.setOnClickListener(this::onClickToggleActivate);
        buttonPrint.setOnClickListener(this::onClickPrint);
        buttonCutPaper.setOnClickListener(this::onClickCutPaper);
        buttonShowIni.setOnClickListener(this::showIni);
        editLog = findViewById(R.id.editExportedIni);

        appDir = getExternalFilesDir(null);

        port = "TCP:192.168.3.200:9100";
        arquivoConfig = appDir.getAbsolutePath() + File.separator + "ACBrPosPrinter.ini";
        setMemoryFile();
        arquivoConfig = memoryFileConfig;
        posPrinter = new ACBrLibPosPrinter(arquivoConfig,"");
       aplicarConfiguracoes();
        Log.i("MainActivity", "arquivoConfig: " + arquivoConfig);


    }

    public void onClickToggleActivate(View v) {
        if (!isActivate){
            try {
                posPrinter.ativar();

            } catch (Exception e) {
                Log.e("MainActivity", "Error activating printer", e);
                Toast.makeText(this, "Error activating printer " +e.getMessage(), Toast.LENGTH_SHORT).show();
            }
            isActivate = true;
            buttonToggleActivate.setText("Desativar");
        } else {
            try {
                posPrinter.desativar();

            } catch (Exception e) {
                Log.e("MainActivity", "Error deactivating printer", e);
            }
            isActivate = false;
            buttonToggleActivate.setText("Ativar");
        }
    }

    public void onClickPrint(View v) {
        // Handle print button click
        String message = "Hello World from JNA";
        try {
            posPrinter.imprimir(message, true, false, false, 10);
            Toast.makeText(this, "Printed: " + message, Toast.LENGTH_SHORT).show();
        }catch (Exception e){
            Toast.makeText(this, "Error printing" + e.getMessage(), Toast.LENGTH_SHORT).show();
            Log.e("MainActivity", "Error printing", e);
        }
    }

    public void onClickCutPaper(View v) {
        // Handle cut paper button click
        try {
            posPrinter.cortarPapel(false);
        }catch (Exception e){
            Log.e("MainActivity", "Error cutting paper", e);
        }
    }

    private void aplicarConfiguracoes(){


        try {
            posPrinter.inicializar();
            posPrinter.configGravarValor("Principal","LogPath", appDir.getAbsolutePath());
            posPrinter.configGravarValor("Principal","LogNivel", "4");
            posPrinter.configGravarValor("PosPrinter", "Porta", port);
            posPrinter.configGravarValor("PosPrinter","Modelo","1");
            posPrinter.configGravarValor("Posprinter","ControlePorta","1");
            posPrinter.configGravar();
        } catch (Exception e) {
            Log.e("MainActivity", "Error applying configurations", e);
        }
    }

    @Override
    protected void onDestroy() {
        Log.i("MainActivity", "onDestroy");
        if (isActivate){
            try {
                posPrinter.desativar();
            } catch (Exception e) {
                Log.e("MainActivity", "Error deactivating printer on destroy", e);

            }
        }
        posPrinter.finalizar();
        super.onDestroy();
    }
    public void showIni(View v){

        String ini = "";

        try {
           ini =  posPrinter.configExportar();
        } catch (Exception e) {
            ini = "Error exporting ini " + e.getMessage() ;
        } finally {
            editLog.setText(ini);
        }
        Log.i("MainActivity", "showIni: " + ini);
    }


   public  void onClickClearCfg(View v){
        File ini = new File(arquivoConfig);
        if ( ini.exists()){
            boolean status =  ini.delete();
            if ( status ){
                Toast.makeText(this, "Arquivo de configuração deletado com sucesso", Toast.LENGTH_SHORT).show();
            } else {
                Toast.makeText(this, "Erro ao deletar arquivo de configuração", Toast.LENGTH_SHORT).show();
            }
        }
    }

    @Override
    public void onBackPressed() {
        super.onBackPressed();
        finish();
    }

    private  void setMemoryFile(){
        memoryFileConfig = "[Memory]\n\n" +
                "[Principal]\n" +
                "LogPath=" +appDir.getAbsolutePath() + "\n" +
                "LogNivel=4\n" +
                "[PosPrinter]\n\n" +
                "Porta=" + port + "\n" +
                "Modelo=1\n" +
                "ControlePorta=0\n" +
                "ArqLog=" +appDir.getAbsolutePath();

    }
}