package com.acbr.cep.acbrlibcep.demo;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import br.com.acbr.lib.cep.ACBrLibCep;

public class ComandosBuscarPorCEPFragment extends Fragment {

    private ACBrLibCep ACBrCEP;

    private EditText txtCEP;
    private EditText txtRespostaConsulta;
    private Button btnBuscarPorCEP;
    private Button btnLimparRespostaConsulta;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View view = inflater.inflate(R.layout.fragment_comandos_buscar_por_cep, container, false);

        ACBrCEP = ACBrLibHelper.getInstance("");
        txtCEP = view.findViewById(R.id.txtCEP);
        txtRespostaConsulta = view.findViewById(R.id.txtRespostaConsulta);
        btnBuscarPorCEP = view.findViewById(R.id.btnBuscarPorCEP);
        btnLimparRespostaConsulta = view.findViewById(R.id.btnLimparRespostaConsulta);

        btnBuscarPorCEP.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                buscarPorCEP();
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

    public void buscarPorCEP(){
        txtRespostaConsulta.setText("");
        String result = "";
        String cep = txtCEP.getText().toString();
        try {
            result = ACBrCEP.buscarPorCep(cep);
        } catch (Exception ex) {
            Log.e("Erro ao Buscar CEP", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void LimparRespostaConsulta(){
        txtRespostaConsulta.setText("");
    }
}