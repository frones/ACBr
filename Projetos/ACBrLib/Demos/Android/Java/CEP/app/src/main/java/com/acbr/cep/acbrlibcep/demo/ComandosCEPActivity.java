package com.acbr.cep.acbrlibcep.demo;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.acbr.cep.acbrlibcep.demo.databinding.ActivityConfiguracoesCepBinding;
import com.google.android.material.tabs.TabLayoutMediator;

import br.com.acbr.lib.cep.ACBrLibCep;

public class ComandosCEPActivity extends AppCompatActivity {

    private ActivityConfiguracoesCepBinding binding;

    private ACBrLibCep ACBrCEP;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_comandos_cep);

        binding = ActivityConfiguracoesCepBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        ACBrCEP = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout(){
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ComandosBuscarPorCEPFragment(), "Buscar Por CEP");
        adapter.addFragment(new ComandosBuscarPorLogradouroFragment(), "Buscar Por Logradouro");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        TabLayoutMediator mediator = new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        });

        mediator.attach();
    }
}