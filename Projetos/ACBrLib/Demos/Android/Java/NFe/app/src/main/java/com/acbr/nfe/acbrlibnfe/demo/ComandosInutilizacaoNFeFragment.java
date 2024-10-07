package com.acbr.nfe.acbrlibnfe.demo;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosInutilizacaoNFeFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private EditText txtRespostaInutilizacao, txtCNPJInutilizarNumeracao, txtJustificativaInutilizarNumeracao, txtAnoInutilizarNumeracao,
            txtModeloInutilizarNumeracao, txtSerieInutilizarNumeracao, txtNumeroInicialInutilizarNumeracao, txtNumeroFinalInutilizarNumeracao;

    private Button btnInutilizarNumeracao, btnLimparRespostaInutilizacao;

    @SuppressLint("MissingInflatedId")
    @Override
    public void onCreate(Bundle savedInstanceState){
        super.onCreate(savedInstanceState);

        // Instância da biblioteca
        ACBrNFe = ACBrLibHelper.getInstance("");
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View view = inflater.inflate(R.layout.fragment_comandos_inutilizacao_nfe, container, false);

        // Inicialização das views após a inflagem do layout
        txtRespostaInutilizacao = view.findViewById(R.id.txtRespostaInutilizacao);
        txtCNPJInutilizarNumeracao = view.findViewById(R.id.txtCNPJInutilizarNumeracao);
        txtJustificativaInutilizarNumeracao = view.findViewById(R.id.txtJustificativaInutilizarNumeracao);
        txtAnoInutilizarNumeracao = view.findViewById(R.id.txtAnoInutilizarNumeracao);
        txtModeloInutilizarNumeracao = view.findViewById(R.id.txtModeloInutilizarNumeracao);
        txtSerieInutilizarNumeracao = view.findViewById(R.id.txtSerieInutilizarNumeracao);
        txtNumeroInicialInutilizarNumeracao = view.findViewById(R.id.txtNumeroInicialInutilizarNumeracao);
        txtNumeroFinalInutilizarNumeracao = view.findViewById(R.id.txtNumeroFinalInutilizarNumeracao);

        btnInutilizarNumeracao = view.findViewById(R.id.btnInutilizarNumeracao);
        btnLimparRespostaInutilizacao = view.findViewById(R.id.btnLimparRespostaInutilizacao);

        btnInutilizarNumeracao.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                inutilizarNumeracao();
            }
        });

        btnLimparRespostaInutilizacao.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v){
                LimparRespostaInutilizacao();
            }
        });

        return view;
    }

    public void inutilizarNumeracao(){
        txtRespostaInutilizacao.setText("");
        String result = "";
        String cnpj = txtCNPJInutilizarNumeracao.getText().toString();
        String justificativa = txtJustificativaInutilizarNumeracao.getText().toString();
        Integer ano = Integer.parseInt(txtAnoInutilizarNumeracao.getText().toString());
        Integer modelo = Integer.parseInt(txtModeloInutilizarNumeracao.getText().toString());
        Integer serie = Integer.parseInt(txtSerieInutilizarNumeracao.getText().toString());
        Integer numeroInicial = Integer.parseInt(txtNumeroInicialInutilizarNumeracao.getText().toString());
        Integer numeroFinal = Integer.parseInt(txtNumeroFinalInutilizarNumeracao.getText().toString());

        try
        {
            result = ACBrNFe.Inutilizar(cnpj, justificativa, ano, modelo, serie, numeroInicial, numeroFinal);
        }
        catch (Exception ex)
        {
            Log.e("Erro ao Enviar NFe", ex.getMessage());
            result = ex.getMessage();
        }
        finally
        {
            txtRespostaInutilizacao.setText(result);
        }
    }

    public void LimparRespostaInutilizacao(){
        txtRespostaInutilizacao.setText("");
    }
}