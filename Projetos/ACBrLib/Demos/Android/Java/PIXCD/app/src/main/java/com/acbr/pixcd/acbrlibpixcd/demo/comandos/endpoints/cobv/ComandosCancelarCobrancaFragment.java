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

public class ComandosCancelarCobrancaFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtTxIdCancelarCobranca;
    private Button btnCancelarCobranca;
    private EditText txtRespostaCancelarCobranca;
    private Button btnLimparRespostaCancelarCobranca;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_cancelar_cobranca, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtTxIdCancelarCobranca = view.findViewById(R.id.txtTxIdCancelarCobranca);
        btnCancelarCobranca = view.findViewById(R.id.btnCancelarCobranca);
        txtRespostaCancelarCobranca = view.findViewById(R.id.txtRespostaCancelarCobranca);
        btnLimparRespostaCancelarCobranca = view.findViewById(R.id.btnLimparRespostaCancelarCobranca);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnCancelarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                CancelarCobranca();
            }
        });

        btnLimparRespostaCancelarCobranca.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaCancelarCobranca();
            }
        });

        return view;
    }

    public void CancelarCobranca(){
        txtRespostaCancelarCobranca.setText("");
        String result = "";
        String txId = txtTxIdCancelarCobranca.getText().toString();
        try{
            result = ACBrPIXCD.CancelarCobrancaImediata(txId);
        } catch (Exception ex) {
            Log.e("Erro ao Cancelar Cobranca", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaCancelarCobranca.setText(result);
        }
    }

    public void LimparRespostaCancelarCobranca(){
        txtRespostaCancelarCobranca.setText("");
    }
}