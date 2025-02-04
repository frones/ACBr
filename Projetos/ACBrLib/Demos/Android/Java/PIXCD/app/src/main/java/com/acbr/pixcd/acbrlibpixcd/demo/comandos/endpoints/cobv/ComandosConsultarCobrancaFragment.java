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

public class ComandosConsultarCobrancaFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtTxIdConsultarCobranca;
    private EditText txtRevisaoConsultarCobranca;
    private Button btnConsultarCobranca;
    private EditText txtRespostaConsultarCobranca;
    private Button btnLimparRespostaConsultarCobranca;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consultar_cobranca, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtTxIdConsultarCobranca = view.findViewById(R.id.txtTxIdConsultarCobranca);
        txtRevisaoConsultarCobranca = view.findViewById(R.id.txtRevisaoConsultarCobranca);
        btnConsultarCobranca = view.findViewById(R.id.btnConsultarCobranca);
        txtRespostaConsultarCobranca = view.findViewById(R.id.txtRespostaConsultarCobranca);
        btnLimparRespostaConsultarCobranca = view.findViewById(R.id.btnLimparRespostaConsultarCobranca);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnConsultarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarCobranca();
            }
        });

        btnLimparRespostaConsultarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarCobranca();
            }
        });

        return view;
    }

    public void ConsultarCobranca(){
        txtRespostaConsultarCobranca.setText("");
        String result = "";
        String txId = txtTxIdConsultarCobranca.getText().toString();
        int revisao = Integer.parseInt(txtRevisaoConsultarCobranca.getText().toString());
        try{
            result = ACBrPIXCD.ConsultarCobranca(txId, revisao);
        } catch (Exception ex){
            Log.e("Erro ao Consultar Cobranca", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarCobranca.setText(result);
        }
    }

    public void LimparRespostaConsultarCobranca(){
        txtRespostaConsultarCobranca.setText("");
    }
}