package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cobv;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosRevisarCobrancaFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtRevisarCobranca;
    private EditText txtTxIdRevisarCobranca;
    private Button btnRevisarCobranca;
    private EditText txtRespostaRevisarCobranca;
    private Button btnLimparRespostaRevisarCobranca;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_revisar_cobranca, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtRevisarCobranca = view.findViewById(R.id.txtRevisarCobranca);
        txtTxIdRevisarCobranca = view.findViewById(R.id.txtTxIdRevisarCobranca);
        btnRevisarCobranca = view.findViewById(R.id.btnRevisarCobranca);
        txtRespostaRevisarCobranca = view.findViewById(R.id.txtRespostaRevisarCobranca);
        btnLimparRespostaRevisarCobranca = view.findViewById(R.id.btnLimparRespostaRevisarCobranca);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnRevisarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                RevisarCobranca();
            }
        });

        btnLimparRespostaRevisarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaRevisarCobranca();
            }
        });

        return view;
    }

    public void RevisarCobranca(){
        txtRespostaRevisarCobranca.setText("");
        String result = "";
        String revisarCobranca = txtRevisarCobranca.getText().toString();
        String txId = txtTxIdRevisarCobranca.getText().toString();
        try{
            result = ACBrPIXCD.RevisarCobranca(revisarCobranca, txId);
        } catch (Exception ex){
            Log.e("Erro ao Revisar Cobranca", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaRevisarCobranca.setText(result);
        }
    }

    public void LimparRespostaRevisarCobranca(){
        txtRespostaRevisarCobranca.setText("");
    }
}