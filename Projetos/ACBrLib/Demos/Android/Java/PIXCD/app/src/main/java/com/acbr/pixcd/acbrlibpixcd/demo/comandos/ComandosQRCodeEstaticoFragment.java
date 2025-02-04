package com.acbr.pixcd.acbrlibpixcd.demo.comandos;

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

public class ComandosQRCodeEstaticoFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtValorQRCodeEstatico;
    private EditText txtInfoAdicionaisQRCodeEstatico;
    private EditText txtTxIdQRCodeEstatico;
    private Button btnGerarQRCodeEstatico;
    private EditText txtRespostaQRCodeEstatico;
    private Button btnLimparRespostaQRCodeEstatico;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_qrcode_estatico, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtValorQRCodeEstatico = view.findViewById(R.id.txtValorQRCodeEstatico);
        txtInfoAdicionaisQRCodeEstatico = view.findViewById(R.id.txtInfoAdicionaisQRCodeEstatico);
        txtTxIdQRCodeEstatico = view.findViewById(R.id.txtTxIdQRCodeEstatico);
        btnGerarQRCodeEstatico = view.findViewById(R.id.btnGerarQRCodeEstatico);
        txtRespostaQRCodeEstatico = view.findViewById(R.id.txtRespostaQRCodeEstatico);
        btnLimparRespostaQRCodeEstatico = view.findViewById(R.id.btnLimparRespostaQRCodeEstatico);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnGerarQRCodeEstatico.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                GerarQRCodeEstatico();
            }
        });

        btnLimparRespostaQRCodeEstatico.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaQRCodeEstatico();
            }
        });

        return view;
    }

    public void GerarQRCodeEstatico(){
        txtRespostaQRCodeEstatico.setText("");
        String result = "";
        double valor = Double.parseDouble(txtValorQRCodeEstatico.getText().toString());
        String informacoesAdicionais = txtInfoAdicionaisQRCodeEstatico.getText().toString();
        String txIdQRCodeEstatico = txtTxIdQRCodeEstatico.getText().toString();
        try {
            result = ACBrPIXCD.GerarQRCodeEstatico(valor, informacoesAdicionais, txIdQRCodeEstatico);
        } catch (Exception ex) {
            Log.e("Erro ao Gerar QRCode Estatico", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaQRCodeEstatico.setText(result);
        }
    }

    public void LimparRespostaQRCodeEstatico(){
        txtRespostaQRCodeEstatico.setText("");
    }
}