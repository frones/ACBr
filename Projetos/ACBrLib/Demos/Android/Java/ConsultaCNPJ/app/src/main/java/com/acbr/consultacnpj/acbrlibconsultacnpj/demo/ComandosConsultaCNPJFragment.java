package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ComandosConsultaCNPJFragment extends Fragment {

    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    private EditText txtConsultarCNPJ;
    private EditText txtRespostaConsulta;
    private Button btnConsultarCNPJ;
    private Button btnLimparRespostaConsulta;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consulta_cnpj, container, false);

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");
        txtConsultarCNPJ = view.findViewById(R.id.txtConsultarCNPJ);
        txtRespostaConsulta = view.findViewById(R.id.txtRespostaConsulta);
        btnConsultarCNPJ = view.findViewById(R.id.btnConsultarCNPJ);
        btnLimparRespostaConsulta = view.findViewById(R.id.btnLimparRespostaConsulta);

        btnConsultarCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarCNPJ();
            }
        });

        btnLimparRespostaConsulta.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsulta();
            }
        });

        return view;
    }

    public void consultarCNPJ(){
        txtRespostaConsulta.setText("");
        String result = "";
        String chave = txtConsultarCNPJ.getText().toString();
        try {
            result = ACBrConsultaCNPJ.consultar(chave);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar CNPJ", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void LimparRespostaConsulta() {
        txtRespostaConsulta.setText("");
    }
}