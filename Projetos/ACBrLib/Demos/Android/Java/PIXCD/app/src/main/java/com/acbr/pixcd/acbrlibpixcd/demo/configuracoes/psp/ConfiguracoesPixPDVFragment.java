package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.psp;

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


public class ConfiguracoesPixPDVFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtCNPJPixPDV;
    private EditText txtTokenPixPDV;
    private EditText txtSecretKeyPixPDV;
    private EditText txtScopesPixPDV;
    private Button btnSalvarConfiguracoesPixPDV;
    private Button btnCarregarConfiguracoesPixPDV;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_pixpdv, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtCNPJPixPDV = view.findViewById(R.id.txtCNPJPixPDV);
        txtTokenPixPDV = view.findViewById(R.id.txtTokenPixPDV);
        txtSecretKeyPixPDV = view.findViewById(R.id.txtSecretKeyPixPDV);
        txtScopesPixPDV = view.findViewById(R.id.txtScopesPixPDV);
        btnSalvarConfiguracoesPixPDV = view.findViewById(R.id.btnSalvarConfiguracoesPixPDV);
        btnCarregarConfiguracoesPixPDV = view.findViewById(R.id.btnCarregarConfiguracoesPixPDV);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnSalvarConfiguracoesPixPDV.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesPixPDV();
            }
        });

        btnCarregarConfiguracoesPixPDV.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesPixPDV();
            }
        });

        carregarConfiguracoesPixPDV();

        return view;
    }

    private void salvarConfiguracoesPixPDV(){
        try{
            ACBrPIXCD.configGravarValor("PixPDV", "CNPJ", txtCNPJPixPDV.getText().toString());
            ACBrPIXCD.configGravarValor("PixPDV", "Token", txtTokenPixPDV.getText().toString());
            ACBrPIXCD.configGravarValor("PixPDV", "SecretKey", txtSecretKeyPixPDV.getText().toString());
            ACBrPIXCD.configGravarValor("PixPDV", "Scopes", txtScopesPixPDV.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações PixPDV: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesPixPDV(){
        try{
            txtCNPJPixPDV.setText(ACBrPIXCD.configLerValor("PixPDV", "CNPJ"));
            txtTokenPixPDV.setText(ACBrPIXCD.configLerValor("PixPDV", "Token"));
            txtSecretKeyPixPDV.setText(ACBrPIXCD.configLerValor("PixPDV", "SecretKey"));
            txtScopesPixPDV.setText(ACBrPIXCD.configLerValor("PixPDV", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações PixPDV: " + ex.getMessage());
        }
    }
}