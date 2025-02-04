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

public class ComandosCancelarCobrancaImediataFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtTxIdCancelarCobrancaImediata;
    private Button btnCancelarCobrancaImediata;
    private EditText txtRespostaCancelarCobrancaImediata;
    private Button btnLimparRespostaCancelarCobrancaImediata;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_cancelar_cobranca_imediata, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtTxIdCancelarCobrancaImediata = view.findViewById(R.id.txtTxIdCancelarCobrancaImediata);
        btnCancelarCobrancaImediata = view.findViewById(R.id.btnCancelarCobrancaImediata);
        txtRespostaCancelarCobrancaImediata = view.findViewById(R.id.txtRespostaCancelarCobrancaImediata);
        btnLimparRespostaCancelarCobrancaImediata = view.findViewById(R.id.btnLimparRespostaCancelarCobrancaImediata);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnCancelarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                CancelarCobrancaImediata();
            }
        });

        btnLimparRespostaCancelarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaCancelarCobrancaImediata();
            }
        });

        return view;
    }

    public void CancelarCobrancaImediata(){
        txtRespostaCancelarCobrancaImediata.setText("");
        String result = "";
        String txId = txtTxIdCancelarCobrancaImediata.getText().toString();
        try{
            result = ACBrPIXCD.CancelarCobrancaImediata(txId);
        } catch (Exception ex) {
            Log.e("Erro ao Cancelar Cobranca Imediata", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaCancelarCobrancaImediata.setText(result);
        }
    }

    public void LimparRespostaCancelarCobrancaImediata(){
        txtRespostaCancelarCobrancaImediata.setText("");
    }
}