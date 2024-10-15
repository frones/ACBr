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

public class ComandosBuscarPorLogradouroFragment extends Fragment {

    private ACBrLibCep ACBrCEP;

    private EditText txtTipo;
    private EditText txtLogradouro;
    private EditText txtCidade;
    private EditText txtUF;
    private EditText txtBairro;
    private EditText txtRespostaConsulta;
    private Button btnBuscarPorLogradouro;
    private Button btnLimparRespostaConsulta;


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_buscar_por_logradouro, container, false);

        ACBrCEP = ACBrLibHelper.getInstance("");
        txtTipo = view.findViewById(R.id.txtTipo);
        txtLogradouro = view.findViewById(R.id.txtLogradouro);
        txtCidade = view.findViewById(R.id.txtCidade);
        txtUF = view.findViewById(R.id.txtUF);
        txtBairro = view.findViewById(R.id.txtBairro);
        txtRespostaConsulta = view.findViewById(R.id.txtRespostaConsulta);
        btnBuscarPorLogradouro = view.findViewById(R.id.btnBuscarPorLogradouro);
        btnLimparRespostaConsulta = view.findViewById(R.id.btnLimparRespostaConsulta);

        btnBuscarPorLogradouro.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                buscarPorLogradouro();
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

    public void buscarPorLogradouro(){
        txtRespostaConsulta.setText("");
        String result = "";
        String tipo = txtTipo.getText().toString();
        String logradouro = txtLogradouro.getText().toString();
        String cidade = txtCidade.getText().toString();
        String uf = txtUF.getText().toString();
        String bairro = txtBairro.getText().toString();

        try{
            result = ACBrCEP.buscarLogradouro(tipo, logradouro, cidade, uf, bairro);
        } catch (Exception ex){
            Log.e("Erro ao Buscar Logradouro", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }

    }

    public void LimparRespostaConsulta(){
        txtRespostaConsulta.setText("");
    }
}