package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cob;

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

public class ComandosCriarCobrancaImediataFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtCriarCobrancaImediata;
    private EditText txtTxIdCriarCobrancaImediata;
    private Button btnCriarCobrancaImediata;
    private EditText txtRespostaCriarCobrancaImediata;
    private Button btnLimparRespostaCriarCobrancaImediata;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_criar_cobranca_imediata, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtCriarCobrancaImediata = view.findViewById(R.id.txtCriarCobrancaImediata);
        txtTxIdCriarCobrancaImediata = view.findViewById(R.id.txtTxIdCriarCobrancaImediata);
        btnCriarCobrancaImediata = view.findViewById(R.id.btnCriarCobrancaImediata);
        txtRespostaCriarCobrancaImediata = view.findViewById(R.id.txtRespostaCriarCobrancaImediata);
        btnLimparRespostaCriarCobrancaImediata = view.findViewById(R.id.btnLimparRespostaCriarCobrancaImediata);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnCriarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                CriarCobrancaImediata();
            }
        });

        btnLimparRespostaCriarCobrancaImediata.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaCriarCobrancaImediata();
            }
        });

        return view;
    }

    public void CriarCobrancaImediata(){
        txtRespostaCriarCobrancaImediata.setText("");
        String result = "";
        String criarCobrancaImediata = txtCriarCobrancaImediata.getText().toString();
        String txId = txtTxIdCriarCobrancaImediata.getText().toString();
        try{
            result = ACBrPIXCD.CriarCobrancaImediata(criarCobrancaImediata, txId);
        } catch (Exception ex){
            Log.e("Erro ao Criar Cobranca Imediata", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaCriarCobrancaImediata.setText(result);
        }
    }

    public void LimparRespostaCriarCobrancaImediata(){
        txtRespostaCriarCobrancaImediata.setText("");
    }
}