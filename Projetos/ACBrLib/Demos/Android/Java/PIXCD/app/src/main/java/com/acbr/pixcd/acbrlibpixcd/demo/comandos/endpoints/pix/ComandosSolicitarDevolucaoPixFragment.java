package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.pix;

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

public class ComandosSolicitarDevolucaoPixFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtSolicitarDevolucaoPIX;
    private EditText txte2eidSolicitarDevolucaoPIX;
    private EditText txtIdDevolucaoSolicitarDevolucaoPIX;
    private Button btnSolicitarDevolucaoPIX;
    private EditText txtRespostaSolicitarDevolucaoPIX;
    private Button btnLimparRespostaSolicitarDevolucaoPIX;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_solicitar_devolucao_pix, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtSolicitarDevolucaoPIX = view.findViewById(R.id.txtSolicitarDevolucaoPIX);
        txte2eidSolicitarDevolucaoPIX = view.findViewById(R.id.txte2eidSolicitarDevolucaoPIX);
        txtIdDevolucaoSolicitarDevolucaoPIX = view.findViewById(R.id.txtIdDevolucaoSolicitarDevolucaoPIX);
        btnSolicitarDevolucaoPIX = view.findViewById(R.id.btnSolicitarDevolucaoPIX);
        txtRespostaSolicitarDevolucaoPIX = view.findViewById(R.id.txtRespostaSolicitarDevolucaoPIX);
        btnLimparRespostaSolicitarDevolucaoPIX = view.findViewById(R.id.btnLimparRespostaSolicitarDevolucaoPIX);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnSolicitarDevolucaoPIX.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                SolicitarDevolucaoPIX();
            }
        });

        btnLimparRespostaSolicitarDevolucaoPIX.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaSolicitarDevolucaoPIX();
            }
        });

        return view;
    }

    public void SolicitarDevolucaoPIX(){
        txtRespostaSolicitarDevolucaoPIX.setText("");
        String result = "";
        String solicitarDevolucaoPIX = txtSolicitarDevolucaoPIX.getText().toString();
        String e2eId = txte2eidSolicitarDevolucaoPIX.getText().toString();
        String idDevolucao = txtIdDevolucaoSolicitarDevolucaoPIX.getText().toString();
        try{
            result = ACBrPIXCD.SolicitarDevolucaoPix(solicitarDevolucaoPIX, e2eId, idDevolucao);
        } catch (Exception ex){
            Log.e("Erro ao Solicitar Devolução PIX", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaSolicitarDevolucaoPIX.setText(result);
        }
    }

    public void LimparRespostaSolicitarDevolucaoPIX(){
        txtRespostaSolicitarDevolucaoPIX.setText("");
    }
}