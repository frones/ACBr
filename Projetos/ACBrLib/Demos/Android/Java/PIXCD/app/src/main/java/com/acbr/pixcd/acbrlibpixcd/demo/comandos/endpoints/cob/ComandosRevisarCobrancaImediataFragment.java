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

public class ComandosRevisarCobrancaImediataFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtRevisarCobrancaImediata;
    private EditText txtTxIdRevisarCobrancaImediata;
    private Button btnRevisarCobrancaImediata;
    private EditText txtRespostaRevisarCobrancaImediata;
    private Button btnLimparRespostaRevisarCobrancaImediata;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_revisar_cobranca_imediata, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtRevisarCobrancaImediata = view.findViewById(R.id.txtRevisarCobrancaImediata);
        txtTxIdRevisarCobrancaImediata = view.findViewById(R.id.txtTxIdRevisarCobrancaImediata);
        btnRevisarCobrancaImediata = view.findViewById(R.id.btnRevisarCobrancaImediata);
        txtRespostaRevisarCobrancaImediata = view.findViewById(R.id.txtRespostaRevisarCobrancaImediata);
        btnLimparRespostaRevisarCobrancaImediata = view.findViewById(R.id.btnLimparRespostaRevisarCobrancaImediata);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnRevisarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                RevisarCobrancaImediata();
            }
        });

        btnLimparRespostaRevisarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaRevisarCobrancaImediata();
            }
        });

        return view;
    }

    public void RevisarCobrancaImediata(){
        txtRespostaRevisarCobrancaImediata.setText("");
        String result = "";
        String revisarCobrancaImediata = txtRevisarCobrancaImediata.getText().toString();
        String txId = txtTxIdRevisarCobrancaImediata.getText().toString();
        try{
            result = ACBrPIXCD.RevisarCobrancaImediata(revisarCobrancaImediata, txId);
        } catch (Exception ex){
            Log.e("Erro ao Revisar Cobranca Imediata", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaRevisarCobrancaImediata.setText(result);
        }
    }

    public void LimparRespostaRevisarCobrancaImediata(){
        txtRespostaRevisarCobrancaImediata.setText("");
    }
}