package com.acbr.nfe.acbrlibnfe.demo.comandos;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosDistribuicaoNFeFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private EditText txtRespostaDistribuicaoDFe, txtUFDFePorChave, txtCNPJDFePorChave, txtChaveDFePorChave,
            txtNSUDFePorNSU, txtUFDFePorNSU, txtCNPJDFePorNSU,
            txtUFDFePorUltNSU, txtCNPJDFePorUltNSU, txtUltNSUDFePorUltNSU;

    private Button btnDFePorChave, btnDFePorNSU, btnDFePorUltNSU, btnLimparRespostaDistribuicaoDFe;

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
        View view = inflater.inflate(R.layout.fragment_comandos_distribuicao_nfe, container, false);

        // Inicialização das views após a inflagem do layout
        txtRespostaDistribuicaoDFe = view.findViewById(R.id.txtRespostaDistribuicaoDFe);
        txtUFDFePorChave = view.findViewById(R.id.txtUFDFePorChave);
        txtCNPJDFePorChave = view.findViewById(R.id.txtCNPJDFePorChave);
        txtChaveDFePorChave = view.findViewById(R.id.txtChaveDFePorChave);
        txtNSUDFePorNSU = view.findViewById(R.id.txtNSUDFePorNSU);
        txtUFDFePorNSU = view.findViewById(R.id.txtUFDFePorNSU);
        txtCNPJDFePorNSU = view.findViewById(R.id.txtCNPJDFePorNSU);
        txtUFDFePorUltNSU = view.findViewById(R.id.txtUFDFePorUltNSU);
        txtCNPJDFePorUltNSU = view.findViewById(R.id.txtCNPJDFePorUltNSU);
        txtUltNSUDFePorUltNSU = view.findViewById(R.id.txtUltNSUDFePorUltNSU);

        btnDFePorChave = view.findViewById(R.id.btnDFePorChave);
        btnDFePorNSU = view.findViewById(R.id.btnDFePorNSU);
        btnDFePorUltNSU = view.findViewById(R.id.btnDFePorUltNSU);
        btnLimparRespostaDistribuicaoDFe = view.findViewById(R.id.btnLimparRespostaDistribuicaoDFe);

        btnDFePorChave.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v){
                dfePorChave();
            }
        });

        btnDFePorNSU.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v){
                dfePorNSU();
            }
        });

        btnDFePorUltNSU.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View v){
                PorUltNSU();
            }
        });

        btnLimparRespostaDistribuicaoDFe.setOnClickListener(new View.OnClickListener(){
            @Override
            public void onClick(View v){
                LimparRespostaDistribuicaoDFe();
            }
        });

        return view;
    }

    public void dfePorChave(){
        txtRespostaDistribuicaoDFe.setText("");
        String result = "";
        Integer uf = Integer.parseInt(txtUFDFePorChave.getText().toString());
        String cnpj = txtCNPJDFePorChave.getText().toString();
        String chave = txtChaveDFePorChave.getText().toString();
        try
        {
            result = ACBrNFe.DistribuicaoDFePorChave(uf, cnpj, chave);
        }
        catch (Exception ex)
        {
            Log.e("Erro DFe Por Chave", ex.getMessage());
            result = ex.getMessage();
        }
        finally
        {
            txtRespostaDistribuicaoDFe.setText(result);
        }
    }

    public void dfePorNSU(){
        txtRespostaDistribuicaoDFe.setText("");
        String result = "";
        Integer uf = Integer.parseInt(txtUFDFePorNSU.getText().toString());
        String cnpj = txtCNPJDFePorNSU.getText().toString();
        String nsu = txtNSUDFePorNSU.getText().toString();
        try
        {
            result = ACBrNFe.DistribuicaoDFePorNSU(uf, cnpj, nsu);
        }
        catch (Exception ex)
        {
            Log.e("Erro DFe Por NSU", ex.getMessage());
            result = ex.getMessage();
        }
        finally
        {
            txtRespostaDistribuicaoDFe.setText(result);
        }
    }

    public void PorUltNSU(){
        txtRespostaDistribuicaoDFe.setText("");
        String result = "";
        Integer uf = Integer.parseInt(txtUFDFePorUltNSU.getText().toString());
        String cnpj = txtCNPJDFePorNSU.getText().toString();
        String nsu = txtUltNSUDFePorUltNSU.getText().toString();
        try
        {
            result = ACBrNFe.DistribuicaoDFePorUltNSU(uf, cnpj, nsu);
        }
        catch (Exception ex)
        {
            Log.e("Erro DFe Por Ult. NSU", ex.getMessage());
            result = ex.getMessage();
        }
        finally
        {
            txtRespostaDistribuicaoDFe.setText(result);
        }
    }

    public void LimparRespostaDistribuicaoDFe(){
        txtRespostaDistribuicaoDFe.setText("");
    }
}