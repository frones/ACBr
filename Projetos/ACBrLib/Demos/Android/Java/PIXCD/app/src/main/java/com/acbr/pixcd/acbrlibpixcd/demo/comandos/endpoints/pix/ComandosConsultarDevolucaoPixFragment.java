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

public class ComandosConsultarDevolucaoPixFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txte2eidConsultarDevolucaoPIX;
    private EditText txtIdDevolucaoConsultarDevolucaoPIX;
    private Button btnConsultarDevolucaoPix;
    private EditText txtRespostaConsultarDevolucaoPix;
    private Button btnLimparRespostaConsultarDevolucaoPix;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consultar_devolucao_pix, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txte2eidConsultarDevolucaoPIX = view.findViewById(R.id.txte2eidConsultarDevolucaoPIX);
        txtIdDevolucaoConsultarDevolucaoPIX = view.findViewById(R.id.txtIdDevolucaoConsultarDevolucaoPIX);
        btnConsultarDevolucaoPix = view.findViewById(R.id.btnConsultarDevolucaoPix);
        txtRespostaConsultarDevolucaoPix = view.findViewById(R.id.txtRespostaConsultarDevolucaoPix);
        btnLimparRespostaConsultarDevolucaoPix = view.findViewById(R.id.btnLimparRespostaConsultarDevolucaoPix);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnConsultarDevolucaoPix.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarDevolucaoPIX();
            }
        });

        btnLimparRespostaConsultarDevolucaoPix.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarDevolucaoPIX();
            }
        });

        return view;
    }

    public void ConsultarDevolucaoPIX(){
        txtRespostaConsultarDevolucaoPix.setText("");
        String result = "";
        String e2eId = txte2eidConsultarDevolucaoPIX.getText().toString();
        String idDevolucao = txtIdDevolucaoConsultarDevolucaoPIX.getText().toString();
        try{
            result = ACBrPIXCD.ConsultarDevolucaoPix(e2eId, idDevolucao);
        } catch (Exception ex){
            Log.e("Erro ao Consultar Devolução PIX", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarDevolucaoPix.setText(result);
        }
    }

    public void LimparRespostaConsultarDevolucaoPIX(){
        txtRespostaConsultarDevolucaoPix.setText("");
    }
}