package com.ben.frameworktesting;

import lombok.Builder;
import lombok.Data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@Builder
@JsonInclude(Include.NON_NULL)
public class Owner {
    
    @JsonProperty("name")
    private final String name;
    
    @JsonProperty("age")
    private final Integer age;
    
    @JsonProperty("irritation_factor")
    private final Integer irritationFactor;
}
