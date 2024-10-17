package com.acbr.nfe.acbrlibnfe.demo.configuracoes;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;

import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ConfiguracoesArquivosFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private CheckBox ckbSalvarArqs;
    private CheckBox ckbPastaMensal;
    private CheckBox ckbAdicionaLiteral;
    private CheckBox ckbEmissaoPathNFe;
    private CheckBox ckbSalvaPathEvento;
    private CheckBox ckbSepararPorCNPJ;
    private CheckBox ckbSepararPorModelo;
    private EditText txtArqNFe;
    private EditText txtArqInu;
    private EditText txtArqEvento;
    private Button btnSalvarConfiguracoesArquivos;
    private Button btnCarregarConfiguracoesArquivos;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_arquivos, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        ckbSalvarArqs = view.findViewById(R.id.ckbSalvarArqs);
        ckbPastaMensal = view.findViewById(R.id.ckbPastaMensal);
        ckbAdicionaLiteral = view.findViewById(R.id.ckbAdicionaLiteral);
        ckbEmissaoPathNFe = view.findViewById(R.id.ckbEmissaoPathNFe);
        ckbSalvaPathEvento = view.findViewById(R.id.ckbSalvaPathEvento);
        ckbSepararPorCNPJ = view.findViewById(R.id.ckbSepararPorCNPJ);
        ckbSepararPorModelo = view.findViewById(R.id.ckbSepararPorModelo);
        txtArqNFe = view.findViewById(R.id.txtArqNFe);
        txtArqInu = view.findViewById(R.id.txtArqInu);
        txtArqEvento = view.findViewById(R.id.txtArqEvento);

        btnSalvarConfiguracoesArquivos = view.findViewById(R.id.btnSalvarConfiguracoesArquivos);
        btnCarregarConfiguracoesArquivos = view.findViewById(R.id.btnCarregarConfiguracoesArquivos);

        btnSalvarConfiguracoesArquivos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesArquivos();
            }
        });

        btnCarregarConfiguracoesArquivos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesArquivos();
            }
        });

        carregarConfiguracoesArquivos();

        return view;
    }

    private void salvarConfiguracoesArquivos() {
        try {
            ACBrNFe.configGravarValor("NFe", "SalvarGer", ckbSalvarArqs.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SepararPorMes", ckbPastaMensal.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "AdicionarLiteral", ckbAdicionaLiteral.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "EmissaoPathNFe", ckbEmissaoPathNFe.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SalvarArq", ckbSalvaPathEvento.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SepararPorCNPJ", ckbSepararPorCNPJ.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SepararPorModelo", ckbSepararPorModelo.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "PathNFe", txtArqNFe.getText().toString());
            ACBrNFe.configGravarValor("NFe", "PathInu", txtArqInu.getText().toString());
            ACBrNFe.configGravarValor("NFe", "PathEvento", txtArqEvento.getText().toString());
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações Arquivos: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesArquivos() {
        try {
            ckbSalvarArqs.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SalvarGer")));
            ckbPastaMensal.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SepararPorMes")));
            ckbAdicionaLiteral.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "AdicionarLiteral")));
            ckbEmissaoPathNFe.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "EmissaoPathNFe")));
            ckbSalvaPathEvento.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SalvarArq")));
            ckbSepararPorCNPJ.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SepararPorCNPJ")));
            ckbSepararPorModelo.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SepararPorModelo")));
            txtArqNFe.setText(ACBrNFe.configLerValor("NFe", "PathNFe"));
            txtArqInu.setText(ACBrNFe.configLerValor("NFe", "PathInu"));
            txtArqEvento.setText(ACBrNFe.configLerValor("NFe", "PathEvento"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Arquivos: " + ex.getMessage());
        }
    }
}