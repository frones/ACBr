package com.acbr.nfe.acbrlibnfe.demo;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;

import androidx.fragment.app.Fragment;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ConfiguracoesEmailFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;
    private EditText txtNome;
    private EditText txtEmail;
    private EditText txtUsuario;
    private EditText txtSenha;
    private EditText txtHost;
    private EditText txtPorta;
    private CheckBox ckbSSL;
    private CheckBox ckbTLS;
    private Button btnSalvarConfiguracoesEmail;
    private Button btnCarregarConfiguracoesEmail;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_email, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        txtNome = view.findViewById(R.id.txtNome);
        txtEmail = view.findViewById(R.id.txtEmail);
        txtUsuario = view.findViewById(R.id.txtUsuario);
        txtSenha = view.findViewById(R.id.txtSenha);
        txtHost = view.findViewById(R.id.txtHost);
        txtPorta = view.findViewById(R.id.txtPorta);
        ckbSSL = view.findViewById(R.id.ckbSSL);
        ckbTLS = view.findViewById(R.id.ckbTLS);
        btnSalvarConfiguracoesEmail = view.findViewById(R.id.btnSalvarConfiguracoesEmail);
        btnCarregarConfiguracoesEmail = view.findViewById(R.id.btnCarregarConfiguracoesEmail);

        btnSalvarConfiguracoesEmail.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesEmail();
            }
        });

        btnCarregarConfiguracoesEmail.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesEmail();
            }
        });

        carregarConfiguracoesEmail();

        return view;
    }

    private void salvarConfiguracoesEmail() {
        try {
            ACBrNFe.configGravarValor("Email", "Nome", txtNome.getText().toString());
            ACBrNFe.configGravarValor("Email", "Conta", txtEmail.getText().toString());
            ACBrNFe.configGravarValor("Email", "Usuario", txtUsuario.getText().toString());
            ACBrNFe.configGravarValor("Email", "Senha", txtSenha.getText().toString());
            ACBrNFe.configGravarValor("Email", "Servidor", txtHost.getText().toString());
            ACBrNFe.configGravarValor("Email", "Porta", txtPorta.getText().toString());
            ACBrNFe.configGravarValor("Email", "SSL", ckbSSL.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("Email", "TLS", ckbTLS.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações Email: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesEmail() {
        try {
            txtNome.setText(ACBrNFe.configLerValor("Email", "Nome"));
            txtEmail.setText(ACBrNFe.configLerValor("Email", "Conta"));
            txtUsuario.setText(ACBrNFe.configLerValor("Email", "Usuario"));
            txtSenha.setText(ACBrNFe.configLerValor("Email", "Senha"));
            txtHost.setText(ACBrNFe.configLerValor("Email", "Servidor"));
            txtPorta.setText(ACBrNFe.configLerValor("Email", "Porta"));
            ckbSSL.setChecked("1".equals(ACBrNFe.configLerValor("Email", "SSL")));
            ckbTLS.setChecked("1".equals(ACBrNFe.configLerValor("Email", "TLS")));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Email: " + ex.getMessage());
        }
    }
}