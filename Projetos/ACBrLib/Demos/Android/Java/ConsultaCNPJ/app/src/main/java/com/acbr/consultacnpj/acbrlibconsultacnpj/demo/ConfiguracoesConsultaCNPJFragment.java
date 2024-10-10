package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Spinner;
import android.widget.TextView;

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;
import br.com.acbr.lib.consultacnpj.Provedor;

public class ConfiguracoesConsultaCNPJFragment extends Fragment {
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    private Spinner cmbProvedor;
    private TextView txtUsuarioProvedor;
    private TextView txtSenhaProvedor;
    private TextView txtProxyServidor;
    private TextView txtProxyPorta;
    private TextView txtProxyUsuario;
    private TextView txtProxySenha;
    private Button btnSalvarConfiguracoesConsultaCNPJ;
    private Button btnCarregarConfiguracoesConsultaCNPJ;

    private Provedor[] provedor = Provedor.values();

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_consulta_cnpj, container, false);

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");

        cmbProvedor = view.findViewById(R.id.cmbProvedor);
        txtUsuarioProvedor = view.findViewById(R.id.txtUsuarioProvedor);
        txtSenhaProvedor = view.findViewById(R.id.txtSenhaProvedor);
        txtProxyServidor = view.findViewById(R.id.txtProxyServidor);
        txtProxyPorta = view.findViewById(R.id.txtProxyPorta);
        txtProxyUsuario = view.findViewById(R.id.txtProxyUsuario);
        txtProxySenha = view.findViewById(R.id.txtProxySenha);

        btnSalvarConfiguracoesConsultaCNPJ = view.findViewById(R.id.btnSalvarConfiguracoesConsultaCNPJ);
        btnCarregarConfiguracoesConsultaCNPJ = view.findViewById(R.id.btnCarregarConfiguracoesConsultaCNPJ);

        SpinnerUtils.preencherSpinner(getContext(), cmbProvedor, provedor);

        btnSalvarConfiguracoesConsultaCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesConsultaCNPJ();
            }
        });

        btnCarregarConfiguracoesConsultaCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesConsultaCNPJ();
            }
        });

        carregarConfiguracoesConsultaCNPJ();

        return view;
    }

    private void salvarConfiguracoesConsultaCNPJ() {
        try{
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Provedor", Integer.toString(cmbProvedor.getSelectedItemPosition()));
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Usuario", txtUsuarioProvedor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Senha", txtSenhaProvedor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Servidor", txtProxyServidor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Porta", txtProxyPorta.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Usuario", txtProxyUsuario.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Senha", txtProxySenha.getText().toString());
            ACBrConsultaCNPJ.configGravar();
        } catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Consulta CNPJ: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesConsultaCNPJ() {
        try{
            cmbProvedor.setSelection(Integer.valueOf(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Provedor")));
            txtUsuarioProvedor.setText(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Usuario"));
            txtSenhaProvedor.setText(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Senha"));
            txtProxyServidor.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Servidor"));
            txtProxyPorta.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Porta"));
            txtProxyUsuario.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Usuario"));
            txtProxySenha.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Senha"));
        } catch (Exception ex){
            Log.i("Erro", " - Carregar Configurações Consulta CNPJ: " + ex.getMessage());
        }
    }

}