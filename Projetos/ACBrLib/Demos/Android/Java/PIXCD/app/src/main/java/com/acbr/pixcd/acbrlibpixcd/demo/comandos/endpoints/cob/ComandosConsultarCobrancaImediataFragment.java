package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cob;

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

public class ComandosConsultarCobrancaImediataFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtTxIdConsultarCobrancaImediata;
    private EditText txtRevisaoConsultarCobrancaImediata;
    private Button btnConsultarCobrancaImediata;
    private EditText txtRespostaConsultarCobrancaImediata;
    private Button btnLimparRespostaConsultarCobrancaImediata;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view  = inflater.inflate(R.layout.fragment_comandos_consultar_cobranca_imediata, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtTxIdConsultarCobrancaImediata = view.findViewById(R.id.txtTxIdConsultarCobrancaImediata);
        txtRevisaoConsultarCobrancaImediata = view.findViewById(R.id.txtRevisaoConsultarCobrancaImediata);
        btnConsultarCobrancaImediata = view.findViewById(R.id.btnConsultarCobrancaImediata);
        txtRespostaConsultarCobrancaImediata = view.findViewById(R.id.txtRespostaConsultarCobrancaImediata);
        btnLimparRespostaConsultarCobrancaImediata = view.findViewById(R.id.btnLimparRespostaConsultarCobrancaImediata);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnConsultarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarCobrancaImediata();
            }
        });

        btnLimparRespostaConsultarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarCobrancaImediata();
            }
        });

        return view;
    }

    public void ConsultarCobrancaImediata(){
        txtRespostaConsultarCobrancaImediata.setText("");
        String result = "";
        String txId = txtTxIdConsultarCobrancaImediata.getText().toString();
        int revisao = Integer.parseInt(txtRevisaoConsultarCobrancaImediata.getText().toString());
        try{
            result = ACBrPIXCD.ConsultarCobrancaImediata(txId, revisao);
        } catch (Exception ex){
            Log.e("Erro ao Consultar Cobranca Imediata", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarCobrancaImediata.setText(result);
        }
    }

    public void LimparRespostaConsultarCobrancaImediata(){
        txtRespostaConsultarCobrancaImediata.setText("");
    }
}