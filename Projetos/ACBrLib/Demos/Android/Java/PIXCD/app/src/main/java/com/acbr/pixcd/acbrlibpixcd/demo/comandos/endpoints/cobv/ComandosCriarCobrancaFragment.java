package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cobv;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.fragment.app.Fragment;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosCriarCobrancaFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtCriarCobranca;
    private EditText txtTxIdCriarCobranca;
    private Button btnCriarCobranca;
    private EditText txtRespostaCriarCobranca;
    private Button btnLimparRespostaCriarCobranca;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_criar_cobranca, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtCriarCobranca = view.findViewById(R.id.txtCriarCobranca);
        txtTxIdCriarCobranca = view.findViewById(R.id.txtTxIdCriarCobranca);
        btnCriarCobranca = view.findViewById(R.id.btnCriarCobranca);
        txtRespostaCriarCobranca = view.findViewById(R.id.txtRespostaCriarCobranca);
        btnLimparRespostaCriarCobranca = view.findViewById(R.id.btnLimparRespostaCriarCobranca);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnCriarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                CriarCobranca();
            }
        });

        btnLimparRespostaCriarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaCriarCobranca();
            }
        });

        return view;
    }

    public void CriarCobranca(){
        txtRespostaCriarCobranca.setText("");
        String result = "";
        String criarCobranca = txtCriarCobranca.getText().toString();
        String txId = txtTxIdCriarCobranca.getText().toString();
        try{
            result = ACBrPIXCD.CriarCobranca(criarCobranca, txId);
        } catch (Exception ex){
            Log.e("Erro ao Criar Cobranca", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaCriarCobranca.setText(result);
        }
    }

    public void LimparRespostaCriarCobranca(){
        txtRespostaCriarCobranca.setText("");
    }
}